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
      lwt wp = Eliom_reference.get wrong_pwd in
      lwt user_n_id = Eliom_reference.get user_n_id in
      (match user_n_id with
        | Some (name,id) ->
            Userpage.page ~cur_user_id:id ~name ~id >|= wrap_main_page
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
  let friend_content o =
    div
      [ div ~a:[a_class ["user-id-placeholder"; "inl-b"; "al-l"; "fl-l"]] [pcdata (Int64.to_string o#id)]
      ; div ~a:[a_class ["user-nick-placeholder"; "inl-b"]]
        [ a ~service:user_service [pcdata o#nick] o#nick
        ]
      ; div ~a:[a_class ["user-exp-placeholder";  "inl-b"; ]] [pcdata (Int32.to_string o#exp)]
    ] |> Lwt.return
  in
  let f = authenticated_handler (fun (nick,id) () () ->
    lwt friends_ids = Db_user.get_friends_by_id id in
    lwt friends_info = Db_user.friends_of_user_by_id ~id in
    lwt friends_content = Lwt_list.map_p friend_content friends_info in
    Eliom_registration.Html5.send
      (wrap_main_page
         [div [div [pcdata (sprintf "friends of user %s(%s) will be listed here" nick (Int64.to_string id))]
              ;br ()
              ;div [pcdata "friends are:"]
              ;div [pcdata (Core_list.to_string  ~f:Int64.to_string friends_ids)]
              ; div friends_content
              ]
         ]
      )
    ) (fun _ () ->
      print_endline "redirection";
      Eliom_registration.Redirection.send main_service
(*        Eliom_registration.Html5.send (wrap_main_page [pcdata "login please"]) *)
  )
  in
  Eliom_registration.Any.register ~service:myfriends_service f

(* Show information about concrete user *)
let _ =
  Eliom_registration.Any.register ~service:user_service (fun name () ->
    Db_user.get_user_by_name name >>= fun is_known ->
    match is_known with
      | Some o ->
          let on_logged_in (_,cur_user_id) =
            Userpage.page ~cur_user_id ~name:o#nick ~id:o#id >|= fun page ->
              wrap_main_page [div page]
          in
          authenticated_handler (fun info () () -> on_logged_in info >>= Eliom_registration.Html5.send )
            (fun _ () -> Eliom_registration.Redirection.send main_service) () ()
      | None   ->
          Eliom_registration.Html5.send
            (html page_head
               (body [h1 [pcdata "404"];
                      p [pcdata "That page does not exist"]]))
    )

let _ =
  Eliom_registration.Action.register
    ~service:connection_service
    (fun () (nick, password) ->
      try
        lwt okay = Db_user.check_password nick password in
        (match okay with
          | Some id -> Eliom_reference.set user_n_id (Some (nick,id))
          | None    -> Eliom_reference.set wrong_pwd true
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
  Eliom_registration.Action.register ~service:append_feed (fun () (text,exp) ->
    print_endline ("appending feed: "^ text);
    Eliom_reference.get user_n_id >|= (function Some x -> x | None -> assert false)
    >>= fun (nick,userid) ->
    Db_user.add_post ~userid ~text ~material_id:Int64.one ~exp
  )
