open All_services
open Eliom_content.Html5.D
open Core.Std

module G = Graph.Imperative.Digraph.Concrete(Int64)
module IM64 = Int64.Map

let describe ~text ~id =
  pcdata (sprintf "%s %s" (Int64.to_string id) text)
let template ~depth ~text ~id xs =
  div ~a:[a_style (sprintf "margin-left: %dpx;" (depth*5))] ((describe ~text ~id):: (br()) :: xs)

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
  Int64.Set.iter roots ~f:(fun x -> f ~depth:0 x |> ignore);
  !visited

let _ =
  Eliom_registration.Any.register ~service:view_skills (fun () () ->
    let g = G.create () in
    let roots = ref Int64.Set.empty in

    lwt skills = Db_user.all_skills () in
    let map_info = List.fold_left ~init:IM64.empty
      ~f:(fun acc x ->
        Ref.replace roots (fun set -> Int64.Set.add set x#id);
        G.add_vertex g x#id;
        IM64.add acc ~key:x#id ~data:(x#descr)
      ) skills
    in

    lwt skill_links = Db_user.get_skill_links () in
    List.iter skill_links ~f:(function
      | (a, Some b) ->
          G.add_edge g b a;
          Ref.replace roots (fun xs -> Int64.Set.remove xs a)
      | (a,None) -> ()
    );
    let m = build !roots
      (fun v -> try IM64.find_exn map_info v with Not_found -> print_endline "PSDA"; raise Not_found) g in

    let ans = !roots |> Int64.Set.to_list |> List.map ~f:(fun id ->
      try IM64.find_exn m id
      with Not_found  -> template ~depth:0 ~text:("ERROR!!!!") ~id []
    ) in
    Eliom_registration.Html5.send
            (html
               (head (title (pcdata "")) [])
               (body [p [pcdata "skills will be here"];
                      div ans
                     ]))
  )
