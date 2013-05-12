open Eliom_content.Html5.D
open Eliom_parameter

open All_services
open Db

(* User names and passwords: *)
let users = ref [("Calvin", "123"); ("Hobbes", "456")]

let user_links () =
  ul (List.map (fun (name, _) -> li [a ~service:user_service [pcdata name] name]) !users)

let check_pwd name pwd = try List.assoc name !users = pwd with Not_found -> false



(* Eliom references *)
let username = Eliom_reference.eref ~scope:Eliom_common.default_session_scope None

let wrong_pwd = Eliom_reference.eref ~scope:Eliom_common.request_scope false



(* Page widgets: *)
let disconnect_box () =
  post_form disconnection_service
    (fun _ -> [fieldset
		  [string_input
                      ~input_type:`Submit ~value:"Log out" ()]]) ()

let create_account_form () =
  post_form ~service:account_confirmation_service
    (fun (name1, name2) ->
      [fieldset
	      [label ~a:[a_for name1] [pcdata "login: "];
           string_input ~input_type:`Text ~name:name1 ();
           br ();
           label ~a:[a_for name2] [pcdata "password: "];
           string_input ~input_type:`Password ~name:name2 ();
           br ();
           string_input ~input_type:`Submit ~value:"Connect" ()
          ]
      ]) ()


let page_head =
  head (title (pcdata ""))
    [ css_link ~uri:(make_uri (Eliom_service.static_dir ()) ["main.css"]) ()
    ]

(* Registration of services *)
let _ =
  Eliom_registration.Html5.register ~service:main_service
    (fun () () ->
      lwt u = Eliom_reference.get username in
      lwt wp = Eliom_reference.get wrong_pwd in
      Lwt.return
        (html page_head (body begin
          match u with
            | Some name -> [
              div ~a:[a_id "main_container"]
                [ Header.search_bar
                ; div ~a:[a_id "contentwrapper" ]
                  [ div ~a:[a_id "contentcolumn"; a_class ["innertube"]] (Userpage.page ~name) ]
                ; Header.menu_bar ~home_service:main_service ~disconnection_service
                ]
            ]
            | None ->
                let l : _ list =
                  [post_form ~service:connection_service
                      (fun (name1, name2) ->
                        [fieldset
		                    [label ~a:[a_for name1] [pcdata "login: "];
                             string_input ~input_type:`Text ~name:name1 ();
                             br ();
                             label ~a:[a_for name2] [pcdata "password: "];
                             string_input ~input_type:`Password ~name:name2 ();
                             br ();
                             string_input ~input_type:`Submit ~value:"Connect" ()
                            ]]) ();
                   p [a new_user_form_service [pcdata "Create an account"] ()]]
                in
                if wp
                then [div ((p [em [pcdata "Wrong user or password"]])::l)]
                else [div l]
        end
         )
        )
    )

let _ =
  Eliom_registration.Html5.register
    ~service:user_service
    (fun name () ->
      if List.exists (fun (n, _) -> n = name) !users
      then begin
        Lwt.return
          (html page_head
             (body [div
               [ Header.search_bar
               ; div [ Header.menu_bar ~home_service:main_service ~disconnection_service
                     ; div (Userpage.page ~name)
                     ]
               ]
              ]
             )
          )
      end
      else
        Lwt.return
          (html page_head
             (body [h1 [pcdata "404"];
                    p [pcdata "That page does not exist"]]))
    );

  Eliom_registration.Action.register
    ~service:connection_service
    (fun () (name, password) ->
      if check_pwd name password
      then Eliom_reference.set username (Some name)
      else Eliom_reference.set wrong_pwd true);

  Eliom_registration.Action.register
    ~service:disconnection_service
    (fun () () -> Eliom_state.discard ~scope:Eliom_common.default_session_scope ());

  Eliom_registration.Html5.register
    ~service:new_user_form_service
    (fun () () ->
      Lwt.return
        (html page_head
              (body [h1 [pcdata "Create an account"];
                     create_account_form ();
                    ])));

  Eliom_registration.Html5.register
    ~service:account_confirmation_service
    (fun () (name, pwd) ->
      let create_account_service =
        Eliom_registration.Action.register_coservice
          ~fallback:main_service
          ~get_params:Eliom_parameter.unit
          ~timeout:60.
          (fun () () ->
            users := (name, pwd)::!users;
            Lwt.return ())
      in
      Lwt.return
        (html page_head
           (body [h1 [pcdata "Confirm account creation for "; pcdata name];
                     p [a ~service:create_account_service [pcdata "Yes"] ();
                        pcdata " ";
                        a ~service:main_service [pcdata "No"] ()]
                    ])));

  Eliom_registration.Html5.register ~service:search_service
    (fun query () ->
      Lwt.return
        (html page_head
           (body [h1 [pcdata "You have searched for "; pcdata query] ]
           )
        )
    )
