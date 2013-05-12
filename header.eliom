open Eliom_content.Html5.D
open Eliom_parameter
open All_services

let svg_title =
  let open Eliom_content.Svg.D in
  [ ellipse ~a:[a_rx (60.,None); a_ry (20.,None); a_cx (60.,None); a_cy (20.,None) ] []
(*  ; text ~a:[a_dx [(20.,Some `Px)]; a_dy [(30., Some `Px)] ] [text [pcdata "text"]] *)

  ]

let search_bar =
  div ~a:[a_id "top_section"]
    [ div ~a:[a_class ["svg_logo"]] [svg svg_title]
(*    ; post_form search_service
      (fun _ -> [fieldset
		            [string_input
                      ~input_type:`Text ~value:"Search" ()]]) ()
*)
    ; div ~a:[a_class ["innertube"] ]
      [h1 [pcdata "CSS Liquid Layout #2.1- (Fixed-Fluid) searchbar"]]
  ]

let menu_bar ~home_service ~disconnection_service =
  div ~a:[a_id "leftcolumn"; a_class ["innertube"]]
    [ a ~a:[a_class ["menubutton"]] ~service:home_service          [pcdata "Home"] ()
(*    ; a ~a:[a_class ["menubutton"]] ~service:disconnection_service [pcdata "Logout"] () *)
    ; div ~a:[a_class ["menubutton"]] [pcdata "Friends"]
    ; div ~a:[a_class ["menubutton"]] [pcdata "Get new skills"]
    ; div ~a:[a_class ["menubutton"]] [pcdata "Devices"]
    ; div ~a:[a_class ["menubutton"]] [pcdata "Settings"]
    ; div ~a:[a_class ["menubutton"]] [pcdata "Groups"]
    ; post_form disconnection_service
              (fun _ -> [fieldset
		                    [string_input
                                ~input_type:`Submit ~value:"Log out" ()]]) ()

    ]
