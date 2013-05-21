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

let myfriends_service = Eliom_service.service ~path:["myfriends"] ~get_params:unit ()

let connection_service =
  Eliom_service.post_service
    ~fallback:main_service
    ~post_params:(string "name" ** string "password")
    ()

let disconnection_service = Eliom_service.post_coservice' ~post_params:unit ()

let new_user_form_service = Eliom_service.service ~path:["create account"] ~get_params:unit ()

let account_confirmation_service =
  Eliom_service.post_coservice ~fallback:new_user_form_service ~post_params:(string "name" ** string "password") ()

let search_service = Eliom_service.service ~path:["search"] ~get_params:(string "query") ()

let append_feed =
  Eliom_service.post_coservice'
    ~post_params: Eliom_parameter.((string "text") ** (int32 "exp") )
                                      (* **
                                      (string "desc") **
                                      (string "tags") ) *)
    ()

let post_wizard = Eliom_service.service ~path:["wizard"] ~get_params:Eliom_parameter.unit ()

(* Eliom session data *)
let user_n_id: (string*int64) option Eliom_reference.eref =
  Eliom_reference.eref ~scope:Eliom_common.default_session_scope None
let wrong_pwd = Eliom_reference.eref ~scope:Eliom_common.request_scope false

let authenticated_handler ok bad =
  Eliom_tools.wrap_handler (fun () -> Eliom_reference.get user_n_id) bad ok
