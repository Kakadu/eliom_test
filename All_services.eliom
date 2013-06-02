open Eliom_content.Html5.D
open Eliom_parameter

{shared{
external (|>): 'a -> ('a -> 'b) -> 'b = "%revapply"
}}
{client{
  let firelog s = Firebug.console##log (Js.string s)
 }}

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

let authenticated_handler ok bad : 'get -> 'post -> 'res Lwt.t =
  Eliom_tools.wrap_handler (fun () -> Eliom_reference.get user_n_id) bad ok

let view_skills =
  Eliom_service.service ~path:["hack_skills"] ~get_params:Eliom_parameter.unit ()


(* Adopting Drup's experience from https://github.com/Drup/evePI/blob/master/auth.ml#L96 *)
(** The Connected Module
	This is module wraps usual services to allow an additionnal argument : the user.
	If the user is not connected, it's redirected to Default_content.v *)

module type Default_content =
sig
  val v : unit -> Html5_types.html Eliom_content.Html5.elt
end

type user = string * int64

module Connected_translate
	(Default : Default_content)
	(App : Eliom_registration.ELIOM_APPL) =
struct
  type page = user -> App.page Lwt.t
  let translate page : App.page Lwt.t =
    match_lwt Eliom_reference.get user_n_id with
      | None -> Lwt.return (Default.v ())
      | Some user -> page user
end

module Connected
	(Default : Default_content )
	(App : Eliom_registration.ELIOM_APPL) =
struct
  include Eliom_registration.Customize
	  ( App )
	  ( Connected_translate (Default) (App) )

  (** Allow to wrap services *)
  module Wrap = struct

	let action_register action =
	  let f =
		Eliom_tools.wrap_handler
		  (fun () -> Eliom_reference.get user_n_id)
		  (fun _ _ -> Lwt.return ())
		  action
	  in
	  Eliom_registration.Action.register f

	let action_with_redir_register ?(redir=Eliom_service.void_coservice') action =
	  let f =
		Eliom_tools.wrap_handler
		  (fun () -> Eliom_reference.get user_n_id)
		  (fun _ _ -> App.send (Default.v ()))
		  (fun u g p ->
			 lwt _ = action u g p in
			 Eliom_registration.Redirection.send redir)
	  in
	  Eliom_registration.Any.register f

	let unit_register action =
	  let f =
		Eliom_tools.wrap_handler
		  (fun () -> Eliom_reference.get user_n_id)
		  (fun _ _ -> Lwt.return ())
		  action
	  in Eliom_registration.Unit.register f

  end

end


module LoginForm = struct
  let v () = Eliom_tools.D.html ~title:"Please login" ~css:[["main.css"]]
    (body
       [div ~a:[a_class ["login_div"]]
           [ post_form ~service:connection_service ~a:[a_class ["login_form"]]
               (fun (name1, name2) ->
                 [fieldset
		             [ div ~a:[a_class ["login_label"]] [label ~a:[a_for name1] [pcdata "Login:"]]
                     ; string_input ~input_type:`Text ~name:name1 (); br ()
                     ; div ~a:[a_class ["login_label"]] [label ~a:[a_for name2] [pcdata "Password:"]]
                     ; string_input ~input_type:`Password ~name:name2 (); br ()
                     ; string_input ~input_type:`Submit ~value:"Connect" ()
                     ]
                 ]) ()
           ; p [a new_user_form_service [pcdata "Create an account"] ()]
           ]
       ]
    )
end

module WithDefault = Connected(LoginForm)(App)

let disconnection_service = Eliom_service.coservice ~fallback:main_service ~get_params:unit ()

let () = WithDefault.Wrap.action_with_redir_register ~service:disconnection_service
  (fun (nick,_) () () ->
    Eliom_state.discard ~scope:Eliom_common.default_session_scope ();
    Lwt.return ()
  )
