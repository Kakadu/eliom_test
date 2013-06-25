open All_services
open Eliom_content.Html5.D
open Printf

{server{
  let add_skill (name,parent_id,maxexp) =
    lwt _ = Db_user.add_skill ~name ~parent_id ~maxexp in
    Lwt.return ()

  let add_skill_rpc : (string*int64*int32, unit) Eliom_pervasives.server_function
      = server_function Json.t<string*int64*int32> add_skill
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

let describe ~text ~id ~maxexp =
  pcdata (sprintf "%s %s(%s)" (Int64.to_string id) text (Int32.to_string maxexp))

let template ~depth ~text ~id ~maxexp xs =
  let onclick = {Dom_html.mouseEvent Js.t -> unit { fun _ ->
    let s = myWin##prompt (Js.string "Enter new material:", Js.string "here") |> Opt.to_option in
    let s'= myWin##prompt (Js.string "Enter maximal expreince:", Js.string "100") |> Opt.to_option in
    try
      let maxexp = match s' with Some s -> Some (s |> Js.to_string |> Int32.of_string) | _ -> None in
      match (s,maxexp) with
        | (Some name, Some maxexp)  ->
            let _name = Js.to_string name in
            Lwt.ignore_result (lwt () = %add_skill_rpc (_name,%id,maxexp) in
                              location##reload () |> Lwt.return)
        | ____________  -> firelog "No data"
    with Failure _ -> firelog "can't convert string to int32"
  }} in
  let add_btn = img ~a:[a_class ["img_btn"]; a_onclick onclick] ~alt:""
    ~src:(make_uri (Eliom_service.static_dir ()) ["archive-insert-3.png"]) () in

  let skill_info  = describe ~text ~id ~maxexp in
  div ~a:[a_style (sprintf "margin-left: %dpx;" (depth*5))] (skill_info :: add_btn :: (br()) :: xs)

module G = Graph.Imperative.Digraph.Concrete(Core_kernel.Std.Int64)

let build roots info g  =
  let visited = ref Core_kernel.Std.Int64.Map.empty in
  let is_visited x = Core_kernel.Std.Int64.Map.mem !visited x in
  let visit ~data key = Core_kernel.Ref.replace visited (Core_kernel.Std.Int64.Map.add ~key ~data) in

  let rec f ~depth (v:int64) : _ Eliom_content.Html5.D.elt =
    if is_visited v then
      let (text,maxexp) = info v in
      template ~depth ~text ~maxexp ~id:v []
    else begin
      visit v ~data:(div []);
      let sons = G.succ g v in
      let (text,maxexp) = info v in
      let data = template ~depth ~text ~maxexp ~id:v (List.map (f ~depth:(depth+1)) sons) in
      visit v ~data;
      data
    end
  in
  Core_kernel.Std.Int64.Set.iter roots ~f:(fun x -> f ~depth:0 x |> ignore);
  !visited

let _ =
  let open Core_kernel.Std in
  WithDefault.register ~service:view_skills (fun  () () -> Lwt.return (fun _ ->
    let g = G.create () in
    let roots = ref Core_kernel.Std.Int64.Set.empty in

    lwt skills = Db_user.all_skills () in
    let map_info = Core_kernel.Std.List.fold_left ~init:Core_kernel.Std.Int64.Map.empty
      ~f:(fun acc x ->
        Core_kernel.Std.Ref.replace roots (fun set -> Core_kernel.Std.Int64.Set.add set x#id);
        G.add_vertex g x#id;
        Core_kernel.Std.Int64.Map.add acc ~key:x#id ~data:(x#descr,x#maxexp)
      ) skills
    in

    lwt skill_links = Db_user.get_skill_links () in
    List.iter skill_links ~f:(function
      | (a, Some b) ->
          G.add_edge g b a;
          Ref.replace roots (fun xs -> Int64.Set.remove xs a)
      | (a,None) -> ()
    );
    let m = g |> build !roots
      (fun (v: int64) ->
        try Core_kernel.Std.Int64.Map.find_exn map_info v
        with Not_found -> print_endline "PSDA"; raise Not_found) in

    let ans = !roots |> Core_kernel.Std.Int64.Set.to_list |> List.map ~f:(fun (id: int64) ->
      try Core_kernel.Std.Int64.Map.find_exn m id
      with Not_found  -> template ~depth:0 ~maxexp:0l ~text:("ERROR!!!!") ~id []
    ) in
    let help_string = "Simple tool for adding skills. in tree below you can see all skills that exist in system. First number is id. Title is in the middle. Last is maximum experience" in
    Lwt.return
            (html
               (head (title (pcdata ""))
                  [ css_link ~uri:(make_uri (Eliom_service.static_dir ()) ["admin.css"]) ()
                  ])
               (body [p [pcdata help_string]; div ans
                     ])
            )
  ))
