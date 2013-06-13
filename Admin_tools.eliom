open All_services
open Eliom_content.Html5.D
open Printf

module G = Graph.Imperative.Digraph.Concrete(Core.Std.Int64)
open Core.Std
module IS64 = Set.Make(Int64)
module IM64 = Map.Make(Int64)

{server{
  let add_skill (name,parent_id) =
    lwt _ = Db_user.add_skill ~name ~parent_id in
    Lwt.return ()

  let add_skill_rpc : (string*int64, unit) Eliom_pervasives.server_function
      = server_function Json.t<string*int64> add_skill
}}

{client{
   external (|>): 'a -> ('a -> 'b) -> 'b = "%revapply"
   open Js
   class type myWindow = object
     method prompt: js_string t -> js_string t -> js_string t opt meth
   end
   let myWin : myWindow t = Js.Unsafe.variable "this"
   let location : Dom_html.location t = Js.Unsafe.variable "location"
   let firelog s = Firebug.console##log (Js.string s)
}}

let describe ~text ~id =
  pcdata (sprintf "%s %s" (Int64.to_string id) text)

let template ~depth ~text ~id xs =
  let onclick = {Dom_html.mouseEvent Js.t -> unit { fun _ ->
    let s = myWin##prompt (Js.string "Enter new material:", Js.string "here") |> Opt.to_option in
    match s with
      | Some name ->
          let _name = Js.to_string name in
          Lwt.ignore_result ((*lwt () = %add_skill_rpc (_name,%id) in
                            location##reload*) () |> Lwt.return)
      | None  -> firelog "No data"
  }} in
  let add_btn = img ~a:[a_class ["img_btn"]; a_onclick onclick] ~alt:""
    ~src:(make_uri (Eliom_service.static_dir ()) ["archive-insert-3.png"]) () in

  div ~a:[a_style (sprintf "margin-left: %dpx;" (depth*5))] ((describe ~text ~id):: add_btn :: (br()) :: xs)

let build roots info g: (_ Eliom_content.Html5.D.elt) IM64.t =
  let visited = ref IM64.empty in
  let is_visited x = IM64.mem !visited x in
  let visit ~data key = visited := IM64.add !visited ~key ~data in

  let rec f ~depth v : _ Eliom_content.Html5.D.elt =
    if is_visited v then
      template ~depth ~text:(info v) ~id:v []
    else begin
      visit v ~data:(div []);
      let sons = G.succ g v in
      let data = template ~depth ~text:(info v) ~id:v (List.map sons ~f:(f ~depth:(depth+1)) ) in
      visit v ~data;
      data
    end
  in
  IS64.iter roots ~f:(fun x -> f ~depth:0 x |> ignore);
  !visited

let _ =
  WithDefault.register ~service:view_skills (fun  () () -> Lwt.return (fun _ ->
    let g = G.create () in
    let roots = ref IS64.empty in

    lwt skills = Db_user.all_skills () in
    let map_info = List.fold_left ~init:IM64.empty
      ~f:(fun acc x ->
        Ref.replace roots (fun set -> IS64.add set x#id);
        G.add_vertex g x#id;
        IM64.add acc ~key:x#id ~data:(x#descr)
      ) skills
    in

    lwt skill_links = Db_user.get_skill_links () in
    List.iter skill_links ~f:(function
      | (a, Some b) ->
          G.add_edge g b a;
          Ref.replace roots (fun xs -> IS64.remove xs a)
      | (a,None) -> ()
    );
    let m = g |> build !roots
      (fun v ->
        try IM64.find_exn map_info v
        with Not_found -> print_endline "PSDA"; raise Not_found) in

    let ans = !roots |> IS64.to_list |> List.map ~f:(fun id ->
      try IM64.find_exn m id
      with Not_found  -> template ~depth:0 ~text:("ERROR!!!!") ~id []
    ) in
    Lwt.return
            (html
               (head (title (pcdata ""))
                  [ css_link ~uri:(make_uri (Eliom_service.static_dir ()) ["admin.css"]) ()
                  ])
               (body [p [pcdata "skills will be here"];
                      div ans
                     ]))
  ))
