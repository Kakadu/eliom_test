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

{server{

  let template ~descr ~query ~exp =
    div ~a:[a_class ["suggestion-item"]]
          [ span ~a:[a_class ["post_tag"]]
              [ span ~a:[a_class ["match"]] [pcdata descr]
              ]
          ; br ()
          ; span ~a:[a_style "item-multiplier"] [pcdata (sprintf "≤ %s" (Int32.to_string exp))]
          ]

  let suggestions query =
    let open Lwt in
    Db.do_search query >|= (List.map (fun o -> template ~query ~descr:o#descr ~exp:o#maxexp))

  let rpc_make_suggestions
      : (string, [ Html5_types.div ] Eliom_content.Html5.D.elt list) Eliom_pervasives.server_function =
    server_function Json.t<string> suggestions
}}

let rec wizard1_handler () () =
  let wizard2_service = App.register_post_coservice
    ~scope:Eliom_common.default_session_scope
    ~fallback:post_wizard
    ~post_params:(Eliom_parameter.string "area_name")
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
              let set_vis s =
                Js.Unsafe.eval_string
                  (sprintf "document.getElementById('azazelle').style.display = '%s';" s)
                          |> ignore in
              %rpc_make_suggestions (Js.to_string query) >|= fun xs -> begin
                match xs with
                  | [] ->
                      set_vis "none";
                      Eliom_content.Html5.Manip.removeAllChild %container
                  | xs ->
                      Eliom_content.Html5.Manip.removeAllChild %container;
                      List.iter (Eliom_content.Html5.Manip.appendChild %container) xs;
                      set_vis "table-cell"
              end
            )
          in
          ()
      }}
    ] in
  Lwt.return (wrap_main_page
                [ h2 [pcdata "Adding post (step 1/3)"]
                ; post_form wizard2_service
                  (fun i ->
                    [ label        ~a:[a_for i] [pcdata "Set the skill area:"]; br ()
                    ; div ~a:[a_class [(*"tag-suggestions"*)]]
                      [ string_input ~a:a_input ~input_type:`Text    ~name:i ()
                      ; button       ~button_type:`Submit [pcdata "Send"]; br ()
                      ; container
                      ]

                    ]
                  ) ()
                ]
  )

and wizard2_handler () area_name =
    let wizard3_service = App.register_post_coservice
        ~scope:Eliom_common.default_session_scope
        ~fallback:post_wizard
        ~post_params:((Eliom_parameter.string "title") **
                      (Eliom_parameter.string "author") **
                      (Eliom_parameter.string "comment") **
                      (Eliom_parameter.int32  "experience")
        )
        (wizard3_handler area_name) in
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
