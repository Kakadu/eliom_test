open Eliom_content.Html5.D
open Eliom_parameter


let search_bar =
  div ~a:[a_id "top_section"] [
    div ~a:[a_class ["innertube"] ]
      [h1 [pcdata "CSS Liquid Layout #2.1- (Fixed-Fluid) searchbar"]]
  ]

let menu_bar ~home_service = div ~a:[a_id "leftcolumn"; a_class ["innertube"]]
  [ b [pcdata "left column"; em [pcdata "200px"]]
  ; div [pcdata "menubar"]
  ; p [a ~service:home_service [pcdata "Home"] ()]
  ]
