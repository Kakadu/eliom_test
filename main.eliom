open Eliom_content.Html5.D
open Eliom_parameter

open All_services
open Db_user

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

(* template for all pages. 1st argumens will is content of user div*)
let wrap_main_page xs =
  html page_head
    (body [div ~a:[a_id "main_container"]
              [ Header.search_bar
              ; div ~a:[a_id "contentwrapper"]
                [ div ~a:[a_id "contentcolumn"; a_class ["innertube"]] xs
                ]
              ; Header.menu_bar ~home_service:main_service ~disconnection_service
              ]
          ]
    )

(* Registration of services *)
let _ =
  Eliom_registration.Html5.register ~service:main_service
    (fun () () ->
      lwt u = Eliom_reference.get username in
      lwt wp = Eliom_reference.get wrong_pwd in
      Lwt.return
        (match u with
            | Some name -> wrap_main_page (Userpage.page ~name)
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
                html page_head (body
                                  (if wp
                                   then [div ((p [em [pcdata "Wrong user or password"]])::l)]
                                   else [div l]
                                  )
                )
        )
    )

let _ =
  Eliom_registration.Html5.register ~service:myfriends_service (fun () () ->
    Lwt.return (wrap_main_page
                  [div [div [pcdata "friends will be listed here"]]]
    )
  )

(* Show information about concrete user *)
let _ =
  Eliom_registration.Html5.register ~service:user_service
    (fun name () ->
      lwt is_known = Db_user.user_exists_by_nick name in
      if is_known
      then begin
        Lwt.return
          (wrap_main_page [div (Userpage.page ~name)])
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
      try
        print_endline "going to execute check_password";
        lwt okay = Db_user.check_password name password in
        print_endline "checking result of okay";
        if okay
        then Eliom_reference.set username (Some name)
        else Eliom_reference.set wrong_pwd true
      with
(*        | (Unix.Unix_error(Unix.ECONNREFUSED, "connect", "")) as exn -> *)
        | exn ->
            print_endline "Exception raised!";
            print_endline (Printexc.to_string exn);
            flush stdout;
            raise exn
    );

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
    (fun () (nick, password) ->
      let create_account_service =
        Eliom_registration.Action.register_coservice
          ~fallback:main_service
          ~get_params:Eliom_parameter.unit
          ~timeout:60.
          (fun () () ->
            Db_user.add_user ~nick ~password ~email:nick ()
          )
      in
      Lwt.return
        (html page_head
           (body [h1 [pcdata "Confirm account creation for "; pcdata nick];
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

let _ =
  Eliom_registration.Html5.register ~service:create_db_service
    (fun () () ->
      Lwt.return
        Eliom_content.Html5.D.(html
                   (head (title(pcdata "DB creation")) [])
                   (body [p [ pcdata "do nothing";
                            ]])))
