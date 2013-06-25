(*
Copyright (c) 2012 Enguerrand Decorne

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

(* adopted from cumulus *)

let (>>=) = Lwt.(>>=)
external (|>): 'a -> ('a -> 'b) -> 'b = "%revapply"

module Lwt_thread = struct
  include Lwt
  include Lwt_chan
end
module Lwt_PGOCaml = PGOCaml_generic.Make(Lwt_thread)
module Lwt_Query = Query.Make_with_Db(Lwt_thread)(Lwt_PGOCaml)

class type ['a, 'b] macaque_type = object
  method get : unit
  method nul : 'b
  method t : 'a
end

type ('a, 'b) t = ('a, 'b) macaque_type Sql.t

module DBSettings = struct
  let database = "traktor"
  let host = "localhost"
  let password = "123"
  let user = "kakadu"
  let port = 5434
end

let connect () : 'a Lwt_PGOCaml.t Lwt.t =
  let open DBSettings in
  Lwt_PGOCaml.connect ~database ~host ~password ~user ~port ()

open Printf

let search_skill_by_descr text =
    lwt dbh = connect () in
    let open Core_kernel.Std in
    let open Lwt_PGOCaml in
    (* TODO check that text is not SQL injection *)
    let query =
      sprintf "select id,descr,maxexp from skills where to_tsvector(descr) @@ to_tsquery('%s')" text  in
    let make a b c = object
      method id = match a with Some a -> Int64.of_string a | _ -> assert false
      method descr  = match b with None -> "" | Some b -> b
      method maxexp = match c with Some c -> Int32.of_string c | None -> assert false
      (* Stupid checks should be validated by compiler in a perfect world *)
    end in
    let s text : _ list monad =
      lwt () = prepare dbh ~query () in
      lwt xs = execute dbh ~params:[] () in
      List.filter_map xs ~f:(function [a;b;c] -> Some (make a b c) | _ -> None) |> return
    in
    let rows = s text in
    rows

let search_in_materials where what =
  lwt dbh = connect () in
  let query =
    sprintf "SELECT id,title,author FROM materials where to_tsvector(%s) @@ to_tsquery('%s')"
      where what in
  printf "query = %s\n%!" query;
  let make a b c =
    ( (match a with Some x -> Int64.of_string x | _ -> assert false),
      (match b with Some s -> s | _ -> ""),
      (match c with Some s -> s | _ -> "")
    ) in
  let f () =
    lwt () = Lwt_PGOCaml.prepare dbh ~query () in
    lwt xs = Lwt_PGOCaml.execute dbh ~params:[] () in
    Core_kernel.Std.List.filter_map xs ~f:(function [a;b;c] -> Some (make  a b c) | _ -> None) |> Lwt.return
  in
  f ()

let search_by_author = search_in_materials "author"
let search_by_title  = search_in_materials "title"


let validate db =
  Lwt.try_bind
    (fun () -> Lwt_PGOCaml.ping db)
    (fun () -> Lwt.return true)
    (fun _ -> Lwt.return false)

let pool: unit Lwt_PGOCaml.t Lwt_pool.t = Lwt_pool.create 1 ~validate connect

let rec in' value = function
  | [] -> (<:value< false >>)
  | x::xs -> (<:value< $x$ = $value$ || $in' value xs$ >>)

let exec f x = Lwt_pool.use pool (fun db -> f db x)

let view x = exec (fun db x -> Lwt_Query.view db x) x
let view_opt x = exec (fun db x -> Lwt_Query.view_opt db x) x
let view_one x = exec (fun db x -> Lwt_Query.view_one db x) x
let query x = exec (fun db x -> Lwt_Query.query db x) x
let value x = exec (fun db x -> Lwt_Query.value db x) x

let alter =
  let name = "query" in
  let low_level_exec db query =
    Lwt_PGOCaml.prepare db ~query ~name () >>= fun () ->
    Lwt_PGOCaml.execute db ~name ~params:[] () >>= fun _ ->
    Lwt_PGOCaml.close_statement db ~name ()
  in
  exec low_level_exec
