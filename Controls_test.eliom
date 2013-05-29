open Main
open Eliom_content.Html5.D
open Printf

let post_wizard = Eliom_service.service ~path:["test1"] ~get_params:Eliom_parameter.unit ()

{shared{
  open Eliom_content.Html5.D
}}

{server{
  type rpc_res_t = (int32 * string * int64) deriving (Json)
  let template (maxexp,descr,_) =
    div ~a:[a_class ["suggestion-item"]]
      [ span ~a:[a_class ["post_tag"]]
          [ span ~a:[a_class ["match"]] [pcdata descr]
          ]
      ; br ()
      ; span ~a:[a_style "item-multiplier"] [pcdata (sprintf "≤ %s" (Int32.to_string maxexp))]
      ] |> Lwt.return


  let suggestions query: rpc_res_t list Lwt.t =
    let open Lwt in
    Db.do_search query  >|= (List.map (fun o -> (o#maxexp,o#descr,o#id)))

  let rpc_make_suggestions
      : (string, rpc_res_t list)
      Eliom_pervasives.server_function =
    server_function Json.t<string> suggestions

  let template_rpc
      : (rpc_res_t, [ Html5_types.div ] Eliom_content.Html5.D.elt)
      Eliom_pervasives.server_function
      = server_function Json.t<rpc_res_t> template
}}

let wizard2_handler () s =
    Lwt.return
    (wrap_main_page [ p  [pcdata "do changes here"]
                    ]
    )

let wizard1_handler () () =
  let wizard2_service = App.register_post_coservice
    ~scope:Eliom_common.default_session_scope
    ~fallback:post_wizard
    ~post_params:(Eliom_parameter.string "name")
    wizard2_handler in

  let sugg_container_id = "azazelle" in
  let container = div ~a:[a_id sugg_container_id; a_class ["tag-suggestions"] ] [] in

  Lwt.return (wrap_main_page
                [ h2 [pcdata "Adding post (step 1/3)"]
                ; post_form wizard2_service
                  (fun (area_name) ->
                    [ label        [pcdata "Wizard1:"]; br ()
                    ; div ~a:[a_class []]
                      [ Controls.text_with_suggestions
                          ~container
                          ~name:area_name
                          ~a:[]
                          rpc_make_suggestions
                          template_rpc
                          {unit->unit{
                            fun _ev -> Firebug.console##log (Js.string "onselected")
                           }}
                      ; button   ~a:[a_id "send_area_btn"] ~button_type:`Submit [pcdata "Use this tag!"]
                      ; br ()
                      ; container
                      ]
                    ]
                  ) ()
                ]
  )


let () =
    App.register ~service:post_wizard wizard1_handler
