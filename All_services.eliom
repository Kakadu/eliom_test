open Eliom_content.Html5.D
open Eliom_parameter

module Traktor_app =
  Eliom_registration.App (
    struct
      let application_name = "traktor"
    end)

(* Services *)
let main_service = Eliom_service.service ~path:[""] ~get_params:unit ()

let user_service =
  Eliom_service.service
    ~path:["users"] ~get_params:(suffix (string "name")) ()

let connection_service =
  Eliom_service.post_service
    ~fallback:main_service
    ~post_params:(string "name" ** string "password")
    ()

let disconnection_service = Eliom_service.post_coservice' ~post_params:unit ()

let new_user_form_service = Eliom_service.service ~path:["create account"] ~get_params:unit ()

let account_confirmation_service =
  Eliom_service.post_coservice ~fallback:new_user_form_service ~post_params:(string "name" ** string "password") ()

let search_service = Eliom_service.service ~path:["search"]
  ~get_params:(string "query") ()
