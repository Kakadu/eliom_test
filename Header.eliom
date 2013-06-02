open Eliom_content.Html5.D
open Eliom_parameter
open All_services

let search_bar =
  div ~a:[a_id "top_section"]
    [ img ~a:[a_class ["svg_logo"]] ~alt:""
        ~src:(make_uri (Eliom_service.static_dir ()) ["logo.png"]) ()
    ; pcdata "Одинокий тракторист"
    ; form ~a:[a_id "search_form"] ~service:search_service (fun name ->
      [fieldset  ~a:[a_id "search_form_fieldset"]
		  [ string_input ~input_type:`Text    ~a:[a_id "search_input"] ~name ~value:"Search" ()
          ; button       ~button_type:`Submit ~a:[a_id "search_btn"]   [pcdata "Publish"]
          ]
      ]
    )
  ]

let menu_bar ~home_service  =
  div ~a:[a_id "leftcolumn"; a_class ["innertube"]]
    [ a   ~a:[a_class ["menubutton"]] ~service:home_service      [pcdata "Home"] (); br ()
    ; a   ~a:[a_class ["menubutton"]] ~service:myfriends_service [pcdata "My Friends"] (); br()
    ; a   ~a:[a_class ["menubutton"]] ~service:post_wizard       [pcdata "Get new skills"] (); br()
    ; div ~a:[a_class ["menubutton"]] [pcdata "Devices"]
    ; div ~a:[a_class ["menubutton"]] [pcdata "Settings"]
    ; div ~a:[a_class ["menubutton"]] [pcdata "Groups"]
    ; a   ~a:[a_class ["menubutton"]] ~service:disconnection_service [pcdata "Log out"] (); br()
    ]
