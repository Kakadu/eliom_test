open All_services
open Eliom_content.Html5.D
open Eliom_parameter
open Main

{shared{
open Printf
}}

{client{
  module Opt = struct
    include Js.Opt
    let (>>=) = bind
    let (>|=) = map
    let of_option = function Some x -> return x | None -> empty
    let (>>>): 'a t -> ('a -> unit) -> unit t = fun x f -> x >|= (fun y -> f y; ())
  end
  module Optdef = struct
    include Js.Optdef
    let (>>=) = bind
    let (>|=) = map
    let of_option = function Some x -> return x | None -> empty
  end
  external (|>): 'a -> ('a -> 'b) -> 'b = "%revapply"
}}
{shared{
  type rpc_res_t =Int64.t * string * ([ Html5_types.div ] Eliom_content.Html5.D.elt)
}}
{server{

  let template ~descr ~query ~exp =
    div ~a:[a_class ["suggestion-item"]]
      [ span ~a:[a_class ["post_tag"]]
          [ span ~a:[a_class ["match"]] [pcdata descr]
          ]
      ; br ()
      ; span ~a:[a_style "item-multiplier"] [pcdata (sprintf "≤ %s" (Int32.to_string exp))]
      ]

  let suggestions query: rpc_res_t list Lwt.t =
    let open Lwt in
    Db.do_search query >|= (List.map (fun o -> (o#id,o#descr,template ~query ~descr:o#descr ~exp:o#maxexp)))

  let rpc_make_suggestions
      : (string, rpc_res_t list)
      Eliom_pervasives.server_function =
    server_function Json.t<string> suggestions
}}

{client{
  let show_suggestions = ref true
  let showhide_element_by_id ~value id =
    Js.Unsafe.eval_string
      (sprintf "document.getElementById('%s').style.display = '%s';" id value) |> ignore

  let set_text_value ~value id =
    Firebug.console##log (Js.string (sprintf "set_text_value of %s to %s" id value));
    Js.Unsafe.eval_string
      (sprintf "document.getElementById('%s').value = '%s';" id value) |> ignore

  let showhide_suggestions value =
    Firebug.console##log (Js.string ("show/hide suggestions:\t" ^ value));
    showhide_element_by_id "azazelle" ~value

  let showhide_submit_btn value =
    showhide_element_by_id "send_area_btn" ~value

  (* this hidden field will be send with form data *)
  let set_hidden_id id : unit = Js.Unsafe.eval_string
    (sprintf "document.getElementById('area_id').value = %s;" (Int64.to_string id)) |> ignore
}}

let rec wizard1_handler () () =
  let wizard2_service = App.register_post_coservice
    ~scope:Eliom_common.default_session_scope
    ~fallback:post_wizard
    ~post_params:(Eliom_parameter.(string "area_name" ** opt (int64 "area_id")))
    wizard2_handler in

  let container = div ~a:[a_id "azazelle"; a_class ["tag-suggestions"] ] [] in
  let a_input =
    [ a_style "width: 500px;"
    ; a_id    "add_tag_input"
    ; a_oninput (* oninput because onchange fires only user moves focus from this box *)
      {{
        fun _ev ->
          let (_: Dom_html.element Js.t Js.optdef) = _ev##target in
          let open Opt in
          let (_ : unit Opt.t) =
            if not !show_suggestions
            then Opt.return ()
            else
            let () = showhide_submit_btn "none" in
            Dom_html.document##getElementById (Js.string "azazelle") >|= fun cont ->
            Lwt.async (fun () ->
              let query =
                let open Opt in
                Dom_html.document##getElementById (Js.string "add_tag_input") >|= fun el ->
                let input: Dom_html.inputElement Js.t = Js.Unsafe.coerce el in
                input##value
              in
              let query = Opt.get query (fun () -> assert false) in
              let open Lwt in
              %rpc_make_suggestions (Js.to_string query) >|= fun xs -> begin
                match xs with
                  | [] ->
                      showhide_suggestions "none";
                      Eliom_content.Html5.Manip.removeAllChild %container
                  | xs ->
                      Eliom_content.Html5.Manip.removeAllChild %container;
                      match xs with
                        | [(id,value,_)] -> begin
                          show_suggestions := false;
                          set_text_value "add_tag_input" ~value;
                          show_suggestions := true;
                          showhide_suggestions "none";
                          set_text_value "area_id" ~value:(Int64.to_string id);
                          showhide_submit_btn "block";
                          set_hidden_id id;
                        end
                        | xs  -> xs |> List.iter (fun (id,name,c) ->
                          (Eliom_content.Html5.To_dom.of_div c)##onclick <- Dom_html.handler (fun _ ->
                            set_text_value "add_tag_input" ~value:name;
                            show_suggestions := false;
                            showhide_suggestions "none";
                            show_suggestions := true;
                            set_text_value "area_id" ~value:(Int64.to_string id);
                            showhide_submit_btn "inline-block";
                            Js.bool true
                           );
                          Eliom_content.Html5.Manip.appendChild %container c;
                          showhide_suggestions "table-cell"
                      )
              end
            )
          in
          ()
      }}
    ] in
  Lwt.return (wrap_main_page
                [ h2 [pcdata "Adding post (step 1/3)"]
                ; post_form wizard2_service
                  (fun (area_name,area_id) ->
                    [ label        [pcdata "Set the skill area:"]; br ()
                    ; int64_input ~a:[a_id "area_id"] ~value:Int64.zero ~name:area_id ~input_type:`Hidden ()
                    ; div ~a:[a_class []]
                      [ string_input ~a:a_input ~input_type:`Text    ~name:area_name ()
                      ; button   ~a:[a_id "send_area_btn"] ~button_type:`Submit [pcdata "Use this tag!"]
                      ; br ()
                      ; container
                      ]
                    ]
                  ) ()
                ]
  )

and wizard2_handler () (area_name, area_id) =
    let wizard3_service = App.register_post_coservice
        ~scope:Eliom_common.default_session_scope
        ~fallback:post_wizard
        ~post_params:((Eliom_parameter.string "title") **
                      (Eliom_parameter.string "author") **
                      (Eliom_parameter.string "comment") **
                      (Eliom_parameter.int32  "experience")
        )
        (wizard3_handler area_name) in
    printf "area_name = %s \n%!" area_name;
    printf "area_id   = %s\n%!" (match area_id with Some x -> Int64.to_string x | None -> "<none>");
    Lwt.return (wrap_main_page
                  [ h2 [pcdata "Step 2/3"]
                  ; p  [pcdata "You've selected area:"; pcdata area_name]
                  ; post_form wizard3_service
                    (fun (title,(author, (comment,exp))) ->
                      [ label [pcdata "Title"]
                      ; string_input ~input_type:`Text  ~name:title  (); br ()
                      ; label [pcdata "Author"]
                      ; string_input ~input_type:`Text  ~name:author (); br ()
                      ; label [pcdata "Comment"]
                      ; string_input ~input_type:`Text  ~name:comment (); br ()
                      ; label [pcdata "Experience"]
                      ; int32_input ~input_type:`Number ~name:exp    (); br ()
                      ; button       ~button_type:`Submit [pcdata "Send"]
                      ]
                    ) ()
                  ]
    )

and wizard3_handler area_name () (title,(author, (comment,exp))) =
    let wizard4_service = App.register_post_coservice
        ~scope:Eliom_common.default_session_scope
        ~fallback:post_wizard
        ~post_params:(Eliom_parameter.unit)
        (wizard4_handler area_name title author exp) in
    Lwt.return
        (wrap_main_page
           [ post_form wizard4_service (fun () ->
             [ p [pcdata "Preview post here"]
             ; p [pcdata (Printf.sprintf "area_name = %s" area_name)]
             ; p [pcdata (Printf.sprintf "title     = %s" title)]
             ; p [pcdata (Printf.sprintf "author    = %s" author)]
             ; p [pcdata (Printf.sprintf "comment   = %s" comment)]
             ; p [pcdata (Printf.sprintf "exp       = %s" (Int32.to_string exp))]
             ; button       ~button_type:`Submit [pcdata "Publish"]
             ]
             ) ()
           ]
        )

and wizard4_handler area_name title author exp () () =
  let a =
    [
      (a_onclick {{ fun _ -> Firebug.console##log (Js.string "111") }})
    ]
  in
  Lwt.return
    (wrap_main_page [ p ~a [pcdata "do changes here"]
                    ]
    )

let () =
    App.register ~service:post_wizard wizard1_handler

{client{

}}
