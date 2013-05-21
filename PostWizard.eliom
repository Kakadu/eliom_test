open All_services
open Eliom_content.Html5.D
open Eliom_parameter
open Main

let make_form i =
    [
    label ~a:[a_for i] [pcdata "Enter an integer:"];
    int_input ~input_type:`Text ~name:i ();
    button ~button_type:`Submit [pcdata "Send"];
    ]

let rec wizard1_handler () () =
    let wizard2_service = App.register_post_coservice
        ~scope:Eliom_common.default_session_scope
        ~fallback:post_wizard
        ~post_params:(Eliom_parameter.string "area_name")
        wizard2_handler in

    Lwt.return (wrap_main_page
                  [ h2 [pcdata "Adding post (step 1/3)"]; br ()
                  ; post_form wizard2_service
                    (fun i ->
                      [ label ~a:[a_for i] [pcdata "Set the skill area:"]; br ()
                      ; string_input ~input_type:`Text    ~name:i ()
                      ; button       ~button_type:`Submit [pcdata "Send"];
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
