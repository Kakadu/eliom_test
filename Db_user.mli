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

(*
type password
val to_password : string -> password

val get_user_name_and_email_with_id :
  int32 ->
  < email : (Sql.string_t, Sql.non_nullable) Db.t;
  name : (Sql.string_t, Sql.non_nullable) Db.t >
    Lwt.t

val get_user_with_name :
  string ->
  < email : (Sql.string_t, Sql.non_nullable) Db.t;
  id : (Sql.int32_t, Sql.non_nullable) Db.t;
  name : (Sql.string_t, Sql.non_nullable) Db.t;
  password : password;
  is_admin : (Sql.bool_t, Sql.non_nullable) Db.t;
  feeds_per_page : (Sql.int32_t, Sql.non_nullable) Db.t >
    option Lwt.t

val get_user_id_with_name :
  string ->
  < id : (Sql.int32_t, Sql.non_nullable) Db.t > Lwt.t
  *)
val get_user_by_name: string -> < id: int64; nick:string; password: string > option Lwt.t
val user_exists_by_nick: string -> bool Lwt.t
val check_password: string -> string -> int64 option Lwt.t
val check_password_bool: string -> string -> bool Lwt.t

val get_friends_by_id: int64 -> int64 list Lwt.t
val friends_of_user_by_id: id:int64 -> < exp : int32; id : int64; nick : string > list Lwt.t

val add_user :
  nick:string ->
  password:string ->
  email:string ->
  unit ->
  unit Lwt.t
(*
val update_user_password :
  userid:int32 ->
  password:password ->
  unit ->
  unit Lwt.t

val update_user_email :
  userid:int32 ->
  email:string ->
  unit ->
  unit Lwt.t

val update_user_feeds_per_page :
  userid:int32 ->
  nb_feeds:int32 ->
  unit ->
  unit Lwt.t
*)
val select_posts_of_user: int64 ->
  < comments : string; date_of_creation : Sql_base.timestamp; exp : int32; id : int64; material_id : int64 >
         Core.Core_list.t Lwt.t

val add_post: userid:int64 -> text: string -> exp:int32 -> material_id:int64 -> unit Lwt.t

val all_skills: unit -> <id: int64; descr: string > list Lwt.t
val get_skill_links: unit -> (int64*(int64 option)) list Lwt.t
val find_material: author:string -> title:string  -> int64 option Lwt.t
val add_material:
  title:string -> author:string -> ?profit:Int32.t -> ?sort_id:Int64.t ->
  ?exp:int32 -> skill_id:int64 -> unit Lwt.t
val last_inserted_material_id: unit -> int64
