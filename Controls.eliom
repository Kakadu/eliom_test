open All_services

{shared{
  open Eliom_content.Html5.D
}}

{client{
  open Eliom_content.Html5
  open Printf
  external (|>): 'a -> ('a -> 'b) -> 'b = "%revapply"
  let hide_elt e = Eliom_content.Html5.Manip.SetCss.display e "none"
  let show_elt e = Eliom_content.Html5.Manip.SetCss.display e "block"
  let firelog s = Firebug.console##log (Js.string s)
}}

let text_with_suggestions ~container ~name ~a get_suggestions template on_show_suggestions_div on_selected =
  let ans = string_input ~name ~a ~input_type:`Text () in

  ignore {unit{
    let my_input = To_dom.of_input %ans in
	let container = %container in
	let get_suggestions = %get_suggestions in
    let on_selected = %on_selected in
    let template = %template in

    let on_text_input _ _ : unit Lwt.t =
      firelog("on_text_input");
      Lwt.async (fun () ->
        firelog (sprintf "Getting suggesions for %s" (Js.to_string my_input##value));
        lwt suggestions = get_suggestions (my_input##value) in
        let () =
          match suggestions with
          | [] -> (* no suggestions *)
              hide_elt container;
              (* we need report that entered data is bad nad we should hide, for example, submit button  *)
              %on_show_suggestions_div ()
          | [(_,text,_) as data] ->
              hide_elt container;
              my_input##value <- Js.string text;
              on_selected data
          | suggestions ->
              Eliom_content.Html5.Manip.removeAllChild container;
              show_elt container;
              %on_show_suggestions_div ();
              (*firelog (sprintf "suggestions count = %d" (List.length suggestions));*)
              (lwt divs = Lwt_list.map_s (fun x -> lwt y = template x in Lwt.return (x,y)) suggestions in
               Lwt_list.iter_s (fun (data,d) ->
                 let (_,descr,id) = data in
                 let () =
                   try
                     Eliom_content.Html5.Manip.appendChild container d
                   with exn -> firelog (sprintf "PIZDA %s" (Printexc.to_string exn) )
                 in
                 Lwt_js_events.mouseups (To_dom.of_element d) (fun _ _ ->
                   my_input##value <- Js.string descr;
                   hide_elt container;
                   on_selected data;
                   Lwt.return ()
                 ) |> Lwt.ignore_result;
                 Lwt.return ()
               ) divs
              ) |> Lwt.ignore_result
         in
         Lwt.return ()
      );
      Lwt.return ()
    in
    begin(*
      lwt () = Lwt_js_events.(keypresses ~use_capture:true my_input (fun ev _ ->
        firelog (sprintf "here %d, text = %s" (ev##keyCode) (Js.to_string my_input##value) );
        Lwt.return (if ev##keyCode <> 13 then Dom_html.stopPropagation ev)
      )) in *)
      Lwt_js_events.inputs my_input on_text_input
    end |> Lwt.ignore_result
  }};
  ans
(*
(* In previous control user should select one of suggestions.
 * There he can enter new data
 *)
let text_with_optional_suggestions
  ~container ~name ~a get_suggestions template on_show_suggestions_div on_selected =
  let ans = string_input ~name ~a ~input_type:`Text () in

  ignore {unit{
    let my_input = To_dom.of_input %ans in
	let container = %container in
	let get_suggestions = %get_suggestions in
    let template = %template in

    let on_text_input _ _ : unit Lwt.t =
      Lwt.async (fun () ->
        firelog (sprintf "Getting suggesions for %s" (Js.to_string my_input##value));
        lwt suggestions = get_suggestions (my_input##value) in
        let () =
          match suggestions with
          | [] -> (* no suggestions *)
              hide_elt container;
              (* we need report that entered data is bad nad we should hide, for example, submit button  *)
              %on_show_suggestions_div ()
          | suggestions ->
              Eliom_content.Html5.Manip.removeAllChild container;
              show_elt container;
              %on_show_suggestions_div ();
              (*firelog (sprintf "suggestions count = %d" (List.length suggestions));*)
              (lwt divs = Lwt_list.map_s (fun x -> lwt y = template x in Lwt.return (x,y)) suggestions in
               Lwt_list.iter_s (fun (data,d) ->
                 Eliom_content.Html5.Manip.appendChild container d;
                 firelog "herrr";(*
                 Lwt_js_events.mouseups (To_dom.of_element d) (fun _ _ ->
                   firelog "2222";
                   hide_elt container;
                   firelog "call on_selected";
                   %on_selected my_input data;
                   Lwt.return ()
                 ) |> Lwt.ignore_result; *)
                 firelog "3333";
                 Lwt.return ()
               ) divs
              ) |> Lwt.ignore_result
         in
         Lwt.return ()
      );
      Lwt.return ()
    in
    let on_blurs _ _ = hide_elt %container; Lwt.return () in
    let () =  Lwt_js_events.inputs my_input on_text_input |> Lwt.ignore_result in
    let () =  Lwt_js_events.blurs  my_input on_blurs  |> Lwt.ignore_result in
    ()
  }};
  ans
*)
