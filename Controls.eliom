open All_services


{client{
  open Printf
  external (|>): 'a -> ('a -> 'b) -> 'b = "%revapply"
  let hide_elt e = Eliom_content.Html5.Manip.SetCss.display e "none"
  let show_elt e = Eliom_content.Html5.Manip.SetCss.display e "block"
}}

let text_with_suggestions ~id ~sugg_container_id ~name ~attribs get_suggestions template on_select =
  let validate_input = {{ fun ev ->
    let e = Dom_html.document##getElementById (Js.string %id) in
    Js.Opt.case e
      (fun () -> Firebug.console##error (Js.string(sprintf "can't get element with id='%s'" %id)))
      (fun e ->
        let e: Dom_html.inputElement Js.t = Js.Unsafe.coerce e in
        let (_: unit Lwt.t) =
          lwt suggestions = %get_suggestions (e##value) in
          if List.length suggestions = 1
          then begin
            e##value <- (List.hd suggestions)#text |> Js.string;
            let () = %on_select (List.hd suggestions) in ()
          end else begin
            Js.Opt.case (Dom_html.document##getElementById (Js.string %sugg_container_id))
              (fun () -> Firebug.console##error
                (Js.string(sprintf "can't get holder for suggestions, id='%s'" %id)) )
              (fun cont ->
                let elt = Eliom_content.Html5.Of_dom.of_element cont in
                Eliom_content.Html5.Manip.removeAllChild elt;
                let divs = List.map %template suggestions in
                divs |> List.iter (fun d ->
                  Eliom_content.Html5.Manip.appendChild elt d;
                  let d' = Eliom_content.Html5.To_dom.of_element d in
                  d'##onmouseup <- Dom_html.handler (fun _ ->
                    Firebug.console##log (Js.string "x");
                    Js.bool true
                  )
                )
              );
          end;
          Lwt.return ()
        in
        ()
      )
  }} in
  Eliom_content.Html5.D.(string_input ~name ~a:((a_oninput validate_input) :: attribs) ~input_type:`Text () )
