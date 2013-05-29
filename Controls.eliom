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

let text_with_suggestions ~container ~name ~a get_suggestions template on_selected =
  let ans = string_input ~name ~a ~input_type:`Text () in

  ignore {unit{
    let my_input = To_dom.of_input %ans in
	let container = %container in
	let get_suggestions = %get_suggestions in
    let on_selected = %on_selected in
    let template = %template in

    let on_text_input _ _ : unit Lwt.t =
      Lwt.async (fun () ->
        lwt suggestions = get_suggestions (my_input##value) in
        let () =
          match suggestions with
          | [] -> (* no suggestions *)
              hide_elt container
          | [(_,text,_) as data] ->
              hide_elt container;
              my_input##value <- Js.string text;
              on_selected data
          | suggestions ->
              Eliom_content.Html5.Manip.removeAllChild container;
              show_elt container;
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
    let _ =  Lwt_js_events.inputs my_input on_text_input in
    ()
  }};
  ans
