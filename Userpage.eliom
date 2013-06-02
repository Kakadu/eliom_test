open Eliom_content.Html5.D
open Eliom_parameter
open Core
open Lwt
open All_services

{shared{
  open Printf
  external (|>): 'a -> ('a -> 'b) -> 'b = "%revapply"
}}
{client{
  open Lwt
  let firelog s = Firebug.console##log (Js.string s)
}}
(* doc about calendar lib: http://calendar.forge.ocamlcore.org/doc/Printer.html *)
let posts_content ~date ~text ~exp =
  div [ div ~a:[a_class ["inl-b"; "post-date-placeholder"]]
          [ pcdata (CalendarLib.Printer.Calendar.sprint "%c" date) ]
      ; div ~a:[a_class ["inl-b"; "post-comment-placeholder"]] [ pcdata text ]
      ; div ~a:[a_class ["inl-b"; "post-exp-placeholder"]] [ pcdata (Int32.format "+%d exp" exp)]; br()
      ]
  |> Lwt.return

let add_friend_rpc =
  let add_friend_link (me, id) = Db_user.add_friend_link ~me id in
  server_function Json.t<int64*int64> add_friend_link

let check_friend_status_rpc =
  let f (me,id) = Db_user.check_friend_status ~me id in
  server_function Json.t<int64*int64> f

let subscribed_div name = div ~a:[a_style ""] [sprintf "you are subscribed on %s" name |> pcdata]
let mutal_div      = div ~a:[a_style ""] [pcdata "you are mutal friends"]
let profile_action_btn_id =  "profile_action_btn"

let make_onclick ~me friend friend_name =
  {Dom_html.mouseEvent Js.t -> unit{ fun _ ->
    Firebug.console##log (Js.string "onclick");
    %add_friend_rpc (%me, %friend) |> Lwt.ignore_result;
    let el = Js.Opt.get
      (Dom_html.document##getElementById (Js.string %profile_action_btn_id))
      (fun () -> assert false) in
    let open Eliom_content.Html5 in
    let e' = Of_dom.of_element el in
    Manip.removeAllChild e';
    firelog "here";
    let () =
      try
        begin
          %check_friend_status_rpc (%friend,%me)
          >|= (function
            | `NoSubscription ->
                D.div [sprintf "you are subscribed on %s" %friend_name |> D.pcdata]
            | `Mutal -> assert false
            | `Subscribed ->
                D.div [sprintf "you are mutal friends" |> D.pcdata])
           >|= fun ans -> Manip.appendChild e' ans
        end |> Lwt.ignore_result
      with exn -> firelog (Printexc.to_string exn)
    in
    firelog "here2"
  }}

let page ~cur_user_id ~name ~id =
  let is_current_user = (cur_user_id = id) in
  Db_user.select_posts_of_user id >>= fun posts ->
  lwt posts_content =
    Lwt_list.map_p (fun o -> posts_content ~date:o#date_of_creation ~text:o#comments ~exp:o#exp) posts
  in
  lwt user_info_tags =
    let avatar = img ~a:[a_class ["user-avatar"]] ~alt:""
      ~src:(make_uri (Eliom_service.static_dir ()) ["demo_avatar.jpg"]) () in
    let info =
      div ~a:[a_class ["inl-b"]]
        [ div ~a:[a_class ["user-text-info-container"]]
            [ div ~a:[a_class []] [pcdata name]
            ; div ~a:[a_class []] [pcdata "много лет"]
            ; div  [pcdata "Из Харцызска"]
            ]
        ]
    in

    if is_current_user then [avatar; info] |> Lwt.return
    else
      lwt bbb = Db_user.check_friend_status ~me:cur_user_id id >>= function
        | `NoSubscription ->
            let d = div ~a:[a_id profile_action_btn_id]
              [ div ~a:[ a_class ["toggle_friend_btn"]
                       ; a_onclick (make_onclick ~me:cur_user_id id name)]
                  [pcdata "Toggle friend"]
              ]
            in Lwt.return d
        | `Mutal      -> Lwt.return mutal_div
        | `Subscribed -> subscribed_div name |> Lwt.return
      in
      let buttons =
        div ~a:[a_id "profile_actions"]
          [ div ~a:[a_id "friend_status"] [ bbb ]
          ] in
      [avatar; info; br (); buttons] |> Lwt.return
  in
  [ div ~a:[a_class ["inl-b"]; a_style ""] user_info_tags
(*
  ; div [b [pcdata "Content column"; em [pcdata (Int64.to_string id)]]]
  ; div [pcdata "some text some text some text some text some text some text some text some text some text some text some text some text some text some text some text some text some text some text some text some text some text some text some text some text some text "] *)
(*  ; div
    (if is_current_user
     then [ div [pcdata "Add new post:"]
          ; post_form ~service:append_feed (fun (text, exp) ->
            [ string_input ~input_type:`Text   ~name:text ()
            ; int32_input  ~input_type:`Number ~name:exp  ()
              ; string_input ~input_type:`Submit ~value:"Add!" ()
            ]) ()
          ]
     else []) *)
  ; br ()
  ; div ~a:[a_style "text-align: center;"] [pcdata "История развития"]; br ()
  ; div posts_content
  (*; progress ~a:[a_id "progress"; a_value "10"; a_maxvalue 100.0] *)
  ] |> Lwt.return
