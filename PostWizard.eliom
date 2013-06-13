open All_services
open Eliom_content.Html5.D
open Eliom_parameter
open Main

{shared{
  open Printf
}}

{client{
  module Opt = struct
    include Js.Opt
    let (>>=) = bind
    let (>|=) = map
    let of_option = function Some x -> return x | None -> empty
    let (>>>): 'a t -> ('a -> unit) -> unit t = fun x f -> x >|= (fun y -> f y; ())
  end
  module Optdef = struct
    include Js.Optdef
    let (>>=) = bind
    let (>|=) = map
    let of_option = function Some x -> return x | None -> empty
  end
  external (|>): 'a -> ('a -> 'b) -> 'b = "%revapply"
  let firelog s = Firebug.console##log (Js.string s)
}}
{shared{
  type rpc_res_skill = Int32.t * string * Int64.t deriving (Json)

  let title_field_id  = "title_field_id"
  let author_field_id = "author_field_id"
  let status_field_id  = "status_field_id"
}}
{server{
  (* About skill suggestions *)
  let template_skill (maxexp, descr,_) =
    div ~a:[a_class ["suggestion-item"]]
      [ span ~a:[a_class ["post_tag"]]
          [ span ~a:[a_class ["match"]] [pcdata descr]
          ]
      ; br ()
      ; span ~a:[a_style "item-multiplier"] [pcdata (sprintf "≤ %s" (Int32.to_string maxexp))]
      ] |> Lwt.return

  let template_skill_rpc
      : (rpc_res_skill, [ Html5_types.div ] Eliom_content.Html5.D.elt)
      Eliom_pervasives.server_function
      = server_function Json.t<rpc_res_skill> template_skill

  let suggestions_skill query: rpc_res_skill list Lwt.t =
    let open Lwt in
    Db.search_skill_by_descr query >|= (List.map (fun o ->
      printf "Suggestions_rpc says: %s\n%!" o#descr;
      (o#maxexp,o#descr,o#id)
    ))

  let suggestions_for_skill_rpc
      : (string, rpc_res_skill list)
      Eliom_pervasives.server_function =
    server_function Json.t<string> suggestions_skill
}}

let hint_label text = label ~a:[a_class ["hint_label"]] [pcdata text]

{client{
  let show_suggestions = ref true
  let showhide_element_by_id ~value id =
    let open Opt in
    case (Dom_html.document##getElementById (Js.string id))
      (fun () -> Firebug.console##error (Js.string (sprintf "element with id='%s' not found" id)))
      (fun e -> Eliom_content.Html5.(Manip.SetCss.display (Of_dom.of_element e) value))

  let set_text_value ~value id =
    Js.Unsafe.eval_string
      (sprintf "document.getElementById('%s').value = '%s';" id value) |> ignore

  let showhide_suggestions value =
    showhide_element_by_id "azazelle" ~value

  let showhide_submit_btn value =
    showhide_element_by_id "send_area_btn" ~value

  (* this hidden field will be send with form data *)
  let set_hidden_id id : unit = Js.Unsafe.eval_string
    (sprintf "document.getElementById('area_id').value = %s;" (Int64.to_string id)) |> ignore

  let (>>>=) s f =
    Js.Opt.case (Dom_html.document##getElementById (Js.string s))
      (fun () -> firelog (sprintf "element with id '%s' not found" s))
        (fun x ->
          let ans: Dom_html.inputElement Js.t  = Js.Unsafe.coerce x in
          f ans
        )
}}

{shared{
  let title_input_id = "post_form_step23_comment"
  let comment_input_id = "post_form_step23_comment"
  let author_input_id = "post_form_step23_comment"
  let exp_input_id = "post_form_step23_comment"
  let material_input_id = "material_input_id"
  let material_status_label = "material_status_label"
}}

{server{
  let check_material (title,author) =
    (* TODO: When we search for material we also need to check it's skill_id *)
    lwt ans = Db_user.find_material ~title ~author in
    let () =
      match ans with
        | Some id -> printf "check_material says: Some %s\n%!" (Int64.to_string id)
        | None -> printf "check_material says: None\n"
    in
    ans |> Lwt.return

  let check_material_rpc
      : (string*string, int64 option) Eliom_pervasives.server_function
      = server_function Json.t<string*string> check_material
}}

let rec wizard1_handler _get _post (*: ('a, 'b) Eliom_registration.kind Lwt.t *) =
  let wizard2_service = App.register_post_coservice
    ~scope:Eliom_common.default_session_scope
    ~fallback:post_wizard
    ~post_params:(Eliom_parameter.(string "area_name" **  (int64 "area_id")))
    wizard2_handler in

  let container = div ~a:[a_id "azazelle"; a_class ["tag-suggestions"] ] [] in
      let submit_form_btn = button ~a:[a_id "send_area_btn"; a_style "visibility: hidden;"]
        ~button_type:`Submit [pcdata "Use this tag!"] in
      let on_show_suggestions_div = {unit->unit{ fun () ->
        firelog "on_show_suggestions_div";
        Eliom_content.Html5.Manip.SetCss.visibility %submit_form_btn "hidden"
      }} in
      let on_selected = {rpc_res_skill->unit{
        fun (_,_,id) ->
          Firebug.console##log (Js.string "onselected");
          Eliom_content.Html5.Manip.SetCss.visibility %submit_form_btn "visible";
          set_text_value "area_id" ~value:(Int64.to_string id);
      }}
      in
      (fun  _ -> wrap_main_page
        [ h2 [pcdata "Adding post (step 1/3)"]
        ; post_form wizard2_service
          (fun (area_name,area_id) ->
            [ label        [pcdata "Set the skill area:"]; br ()
            ; int64_input ~a:[a_id "area_id"] ~value:Int64.zero ~name:area_id ~input_type:`Hidden ()
            ; div ~a:[a_class []]
              [ Controls.text_with_suggestions
                  ~container
                  ~name:area_name
                  ~a:[a_class ["post_wizard_step13_input"]]
                  suggestions_for_skill_rpc
                  template_skill_rpc
                  on_show_suggestions_div
                  on_selected
              ; hint_label "Write 'OCaml' for example"; br()
              ; submit_form_btn
              ; br ()
              ; container
              ]
            ]
          ) ()
        ] |> Lwt.return
      ) |> Lwt.return

and wizard2_handler () (area_name, area_id) =
    let wizard3_service = App.register_post_coservice
        ~scope:Eliom_common.default_session_scope
        ~fallback:main_service
        ~post_params:Eliom_parameter.(
(*          (string "action") ** *)
          (string "title") **
          (string "author") **
          (string "comment") **
          (int32  "experience") **
          (neopt (int64 "material_id"))
        )
        (wizard3_handler area_name area_id) in
    printf "area_name = %s \n%!" area_name;
    printf "area_id   = %s\n%!" (match area_id with x -> Int64.to_string x );

    let submit_btn = button ~button_type:`Submit [pcdata "Send"]
      ~a:[a_id "submit_23"; a_style "visibility: hidden;"] in

    let set_status = {string->unit{ fun s ->
      Js.Opt.map (Dom_html.document##getElementById (Js.string material_status_label)) (fun e ->
        let statusLabel: Dom_html.labelElement Js.t = Js.Unsafe.coerce e in
        e##style##color <- Js.string "black";
        e##style##fontWeight <- Js.string "normal";
        statusLabel##innerHTML <- Js.string s;
        statusLabel##style##visibility <- Js.string "visible"
      ) |> ignore
    }} in
    let set_error_status = {string->unit{ fun s ->
      Js.Opt.map (Dom_html.document##getElementById (Js.string material_status_label)) (fun e ->
        e##style##color <- Js.string "red";
        e##style##fontWeight <- Js.string "bold";
        let statusLabel: Dom_html.labelElement Js.t = Js.Unsafe.coerce e in
        statusLabel##innerHTML <- Js.string s;
        statusLabel##style##visibility <- Js.string "visible"
      ) |> ignore
    }} in
    let set_material_field = {string->unit{ fun v ->
      firelog (sprintf "set_material_field  = %s" v);
      Js.Opt.map (Dom_html.document##getElementById (Js.string material_input_id) ) ( fun e ->
        let e: Dom_html.inputElement Js.t = Js.Unsafe.coerce e in
        e##value <- Js.string v
      ) |> ignore
    }} in
    let showhide_submit = {bool->unit{ function
      | true  -> Eliom_content.Html5.Manip.SetCss.visibility %submit_btn "visible"
      | false -> Eliom_content.Html5.Manip.SetCss.visibility %submit_btn "hidden"
    }} in

    let check_existance_material_helper = {unit->unit{ fun () ->
      (* it will be executed when author and title text fields will lose focus *)
      title_field_id  >>>= fun titleEl ->
      author_field_id >>>= fun authorEl ->
(*      let e =  Dom_html.document##getElementById (Js.string "material_status_label") in *)
      let cur_title  = Js.to_string titleEl##value in
      let cur_author =  Js.to_string authorEl##value in
      if (cur_title = "" || cur_author = "")
      then ()
      else begin
       (let open Lwt in
        %check_material_rpc (cur_title, cur_author) >|= function
          | Some id ->
              %set_material_field (Int64.to_string id);
              %set_status "Using existent material"
          | None    ->
              %set_material_field "";
              %set_status "this is new material"
        ) |> Lwt.ignore_result;
      end
    }} in
    let check_existance_material = {Dom_html.event Js.t -> unit{
      fun _ -> %check_existance_material_helper () }}
    in
    let post_wizard_action_input_id = "post_wizard_action_input" in
    let check_correctness_helper = {unit->unit{ fun () ->
      %post_wizard_action_input_id >>>= fun el ->
      let s = Js.to_string (el##value) in
      firelog s;
      if s = "" then ( %set_error_status "Action can't be empty"; %showhide_submit false )
      else   ( %set_status ""; %showhide_submit true )
    }} in
    let check_correctness = {Dom_html.event Js.t -> unit{ fun _ ->
      firelog ("check_correctness");
      %check_correctness_helper ()
    }}
    in
    let make_label text = label ~a:[a_style "padding-right: 5px;"] [pcdata text] in
    let make_int32_input id name = int32_input ~input_type:`Number  ~value:Int32.one ~name
      ~a:[a_id id; a_class ["post_wizard_step23_input"]] () in

    Lwt.return
      (wrap_main_page
         [ h2 [pcdata (sprintf "Step 2/3: upgrading skill '%s'" area_name)]
         ; post_form ~a:[a_onload check_correctness] ~service:wizard3_service
           (fun ((title,(author, (comment,(exp,material_id_name)))) ) ->
             [ make_label "Action:"
             ; string_input ~input_type:`Text ~value:"xxx" (* ~name:action *)
               ~a:[a_onblur check_correctness; a_id post_wizard_action_input_id] ()
             ; hint_label "done, created, visited..."
             ; br ()
             ; div
               [ div ~a:[a_class ["inl-b"]]
                   [ make_label "Title:"; hint_label "optional"; br ()
                   ; string_input ~name:title ~a:[a_id title_field_id; a_onblur check_existance_material]
                     ~input_type:`Text ()
                   ; br() ]
               ; div ~a:[a_class ["inl-b"]; a_style "margin-left: 50px;"]
                   [ make_label "Author:"; hint_label "optional"; br ()
                   ; string_input ~name:author ~a:[a_id author_field_id]
                     ~input_type:`Text ()
                   ; br() ]
               ]
             ; label ~a:[a_id material_status_label] [pcdata ""]
             ; br()

             ; make_label "Comment:"; hint_label "optional"; br ()
             ; textarea ~name:comment ~a:[a_class ["post_wizard_comment_textarea"]] ()
             ; br()

             ; make_label "Experience:"
             ; make_int32_input exp_input_id exp
             ; submit_btn

             ; int64_input ~a:[a_id material_input_id]  ~name:material_id_name ~input_type:`Hidden ()
             ]
           ) ()
         ]
      )

and wizard3_handler area_name area_id () (title,(author, (comment,(exp,material_id))))  =
  let wizard4_service = Eliom_service.post_coservice
        ~fallback:main_service
        ~post_params: Eliom_parameter.unit ()
  in
  print_endline "wizard3_handler";
  let action = "action" in
  let () = WithDefault.Wrap.action_with_redir_register ~service:wizard4_service
    (wizard4_handler area_id ~title ~action ~comment ~author ~exp material_id)
  in
  lwt preview_div = Userpage.posts_content (object
    method date_of_creation = CalendarLib.(Calendar.create (Date.today ()) (Time.now ()) )
    method comments = comment
    method author   = author
    method title    = title
    method exp      = exp
    method action   = action
  end) in
  Lwt.return
    (wrap_main_page
       [ post_form wizard4_service (fun material_id ->
         [ p [pcdata "Preview post here"]
         ; preview_div
         ; button       ~button_type:`Submit [pcdata "Publish"]
         ]
         ) ()
       ]
    )

and wizard4_handler skill_id ~action ~title ~author ~exp ~comment material_id (nick,userid) () () =
  (lwt material_id =
    match material_id with
      | Some id -> Lwt.return id
      | None ->
          printf "Adding new material\n%!";
          (* We will add new material *)
          Db_user.add_material ~title ~author ~exp ~skill_id ~profit:0l ~sort_id:Int64.zero
   in
   Db_user.add_post ~action ~text:comment ~exp ~material_id ~userid
  ) |> Lwt.ignore_result;
  Eliom_registration.Redirection.send main_service

let () =
    WithDefault.register ~service:post_wizard wizard1_handler
(*    App.register ~service:post_wizard wizard1_handler *)
