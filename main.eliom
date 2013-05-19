open Eliom_content.Html5.D
open Eliom_parameter

open All_services
open Db_user
open Printf
open Eliom_lib
open Core
open Core.Std
open Lwt

external (|>): 'a -> ('a -> 'b) -> 'b = "%revapply"

(* Eliom references *)
let username  = Eliom_reference.eref ~scope:Eliom_common.default_session_scope None
let userid    = Eliom_reference.eref ~scope:Eliom_common.default_session_scope None
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
      lwt id = Eliom_reference.get userid in
        (match u with
            | Some name ->
                Userpage.page ~name ~id:(Option.value_exn id) >|= wrap_main_page

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
                ) |> Lwt.return
        )
    )

let _ =
  Eliom_registration.Html5.register ~service:myfriends_service (fun () () ->
    lwt name = Eliom_reference.get username in
    lwt id = Eliom_reference.get userid in
    let name = Option.value_exn name in
    let id   = Option.value_exn id in
    lwt friends_ids = Db_user.get_friends_by_id id in
    Lwt.return
      (wrap_main_page
         [div [div [pcdata (sprintf "friends of user %s(%s) will be listed here" name (Int64.to_string id))]
              ;br ()
              ;div [pcdata "friends are:"]
              ;div [pcdata (Core_list.to_string  ~f:Int64.to_string friends_ids)]
              ]
         ]
      )
  )

(* Show information about concrete user *)
let _ =
  Eliom_registration.Html5.register ~service:user_service
    (fun name () ->
      (* TODO: это быдлокод *)
      lwt is_known = Db_user.get_user_by_name name in
      (match is_known with
        | Some o -> Userpage.page ~name:o#nick ~id:o#id >|= fun page -> wrap_main_page [div page]
        | None   ->
            Lwt.return
              (html page_head
                 (body [h1 [pcdata "404"];
                        p [pcdata "That page does not exist"]]))
      )
    );

  Eliom_registration.Action.register
    ~service:connection_service
    (fun () (name, password) ->
      try
        lwt okay = Db_user.check_password name password in
        (match okay with
          | Some id ->
              let _ = Eliom_reference.set username (Some name) in
              Eliom_reference.set userid   (Some id)
          | None -> Eliom_reference.set wrong_pwd true
        )
      with
        | exn ->
            print_endline "Exception raised!";
            print_endline (Exn.to_string exn);
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
      (* TODO: check if new user nick is already exists in database *)
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

let _ =
  Eliom_registration.Action.register ~service:append_feed (fun () (text,exp) ->
    print_endline ("appending feed: "^ text);
    lwt id = Eliom_reference.get userid in
    let userid = Option.value_exn id in
(*    let exp = match Int32.of_int exp with Some x -> x | None  -> 1l in *)
    Db_user.add_post ~userid ~text ~material_id:Int64.one ~exp
  )
