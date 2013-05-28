open Main
open Eliom_content.Html5.D
open Printf

let post_wizard = Eliom_service.service ~path:["test1"] ~get_params:Eliom_parameter.unit ()

{server{
  type rpc_res_t =Int64.t * string * ([ Html5_types.div ] Eliom_content.Html5.D.elt)

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
  let get_suggestions s =
    suggestions s
  in

  Lwt.return (wrap_main_page
                [ h2 [pcdata "Adding post (step 1/3)"]
                ; post_form wizard2_service
                  (fun (area_name) ->
                    [ label        [pcdata "Wizard1:"]; br ()
                    ; div ~a:[a_class []]
                      [ (* string_input  ~input_type:`Text    ~name:area_name () *)
                        Controls.text_with_suggestions
                          ~id:"input1"
                          ~sugg_container_id
                          ~name:area_name
                          ~attribs:[]
                          get_suggestions
                          template
                          {unit->unit{
                            fun _ev -> Firebug.console##log (Js.string "111")
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
