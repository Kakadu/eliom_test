open All_services
open Eliom_content.Html5.D


open Core.Std
module G = Graph.Imperative.Digraph.Concrete(Int64)
module Top = Graph.Topological.Make(G)
module IM64 = Int64.Map

let build info g: (_ Eliom_content.Html5.D.elt) IM64.t =
  let visited = ref IM64.empty in
  let is_visited x = IM64.mem !visited x in
  let visit ~data key = visited := IM64.add !visited ~key ~data in
  let describe v = pcdata (sprintf "%s %s" (Int64.to_string v) (info v)) in
  let rec f ~depth v : _ Eliom_content.Html5.D.elt =

    if is_visited v then
      div ~a:[a_style (sprintf "margin-left: %dpx;" (depth*5))] [describe v; br()]
    else begin
      visit v ~data:(div []);
      let sons = G.succ g v in
      let data = div ~a:[a_style (sprintf "padding-left: %dpx;" (depth*5))]
        ((describe v) :: (br()) :: (List.map sons ~f:(f ~depth:(depth+1)) ) ) in
      (*printf "Add data for id=%s\n%!" (Int64.to_string v);*)
      visit v ~data;
      data
    end
  in
  g |> Top.iter (fun x -> f ~depth:0 x |> ignore);
  !visited


let _ =
  Eliom_registration.Any.register ~service:view_skills (fun () () ->
    let g = G.create () in
    let roots = ref Int64.Set.empty in

    lwt skills = Db_user.all_skills () in
    let map_info = List.fold_left ~init:IM64.empty
      ~f:(fun acc x ->
        roots := Int64.Set.add !roots x#id;
        IM64.add acc ~key:x#id ~data:(x#descr)
      ) skills
    in

    lwt skill_links = Db_user.get_skill_links () in
    List.iter skill_links ~f:(function
      | (a, Some b) ->
          G.add_vertex g a;
          G.add_vertex g b;
          G.add_edge   g b a;
          Ref.replace roots (fun xs -> Int64.Set.remove xs a)
      | (a,None) ->
          G.add_vertex g a
    );

    let m = build (fun v -> IM64.find_exn map_info v) g in
    let ans = !roots |> Int64.Set.to_list |> List.map ~f:(fun id -> IM64.find_exn m id) |> div in
    Eliom_registration.Html5.send
            (html
               (head (title (pcdata "")) [])
               (body [p [pcdata "skills will be here"];
                      ans
                     ]))
  )
