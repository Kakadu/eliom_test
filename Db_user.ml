open Printf
module Option = Eliom_lib.Option

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)
external (|>): 'a -> ('a -> 'b) -> 'b = "%revapply"

let users_id_seq = (<:sequence< bigserial "users_id_seq" >>)

let users = (<:table< users (
  id              bigint    NOT NULL DEFAULT(nextval $users_id_seq$),
  nick            text      NOT NULL,
  email           text      NOT NULL,
  password_digest text      NOT NULL,
  exp             integer   NOT NULL
) >>)

let friends = (<:table< friends (
  user_id        bigint NOT NULL,
  friend_id      bigint NOT NULL
) >>)

open Core

let get_user_id_with_name name =
  Db.view_one
    (<:view< {
      u.id
     } | u in $users$;
    u.nick = $string:name$;
    >>)

let get_user_name_and_email_with_id id =
  Db.view_one
    (<:view< {
      u.nick;
      u.email;
     } | u in $users$;
    u.id = $int64:id$;
    >>)

let get_user_by_name name : _ option Lwt.t =
  Db.view_opt
    (<:view< {
      u.id;
      u.nick;
      u.password_digest;
      u.email;
     } | u in $users$;
    u.nick = $string:name$;
    >>)
  >|= (Option.map
       ~f:(fun x ->
           object
             method id = x#!id;
             method nick = x#!nick;
             method password = x#!password_digest;
           end
          )
      )

let get_friends_by_id id : int64 list Lwt.t =
  Db.query <:select< {user_id = x.user_id; friend_id = x.friend_id } | x in $friends$; >>
  >|= (Core_list.filter_map ~f:(fun x -> if x#!user_id = id then Some (x#!friend_id) else None))

let check_friend_status ~me friend_id =
  lwt left = Db.view_opt
    (<:view< x | x in $friends$; x.user_id = $int64:me$; x.friend_id = $int64:friend_id$ >>)
    >|= Option.is_some in
  if left then begin
    lwt right = Db.view_opt
      (<:view< x | x in $friends$; x.user_id = $int64:friend_id$; x.friend_id = $int64:me$ >>)
      >|= Option.is_some in
    Lwt.return (if right then `Mutal else `Subscribed)
  end else Lwt.return `NoSubscription

let add_friend_link ~me id =
  Db.query (<:insert< $friends$ := { user_id = $int64:me$; friend_id = $int64:id$ } >>)

(* TODO: make select * from users  JOIN friends ON (users.id=friends.user_id);
    somehow *)

let friends_of_user_by_id ~id : _ Lwt.t =
  Db.view (<:view< x (*order by exp desc*)
              | x in $users$; y in $friends$; y.user_id = $int64:id$; x.id = y.friend_id >>)
  >|= (Core_list.map ~f:(fun o -> object
    method id = o#!id
    method nick = o#!nick
    method exp = o#!exp
  end))

let user_exists_by_nick nick =
  Db.view_opt
    (<:view< {
      u.id;
      u.nick;
     } | u in $users$;
    u.nick = $string:nick$;
    >>)
  >>= fun user ->
  Lwt.return (match user with Some _ -> true | None -> false)

let check_password nick password =
  Db.view_opt
    (<:view< {
      u.password_digest;
      u.nick;
      u.id;
     } | u in $users$;
    u.nick = $string:nick$;
    u.password_digest = $string:password$
    >>)
  >|=  (Option.map ~f:(fun x -> x#!id))

let check_password_bool nick password =
  check_password nick password >|= (function Some _ -> true | None -> false)

let add_user ~nick ~password ~email () =
  Db.query
    (<:insert< $users$ := {
      id = nextval $users_id_seq$;
      nick = $string:nick$;
      email = $string:email$;
      password_digest = $string:password$;
      exp = $int32:0l$;
    } >>)

(*
let update_user_password ~userid ~password () =
  Db.query
    (<:update< u in $users$ := {
      password = $string:Bcrypt.string_of_hash password$;
    } | u.id = $int32:userid$; >>)

let update_user_email ~userid ~email () =
  Db.query
    (<:update< u in $users$ := {
      email = $string:email$;
    } | u.id = $int32:userid$; >>)

let update_user_feeds_per_page ~userid ~nb_feeds () =
  Db.query
    (<:update< u in $users$ := {
      feeds_per_page = $int32:nb_feeds$;
    } | u.id = $int32:userid$; >>)
*)

let posts_id_seq = (<:sequence< bigserial "posts_id_seq" >>)
let last_inserted_post_id (): int64 =
  let open Db.DBSettings in
  let dbh = Query.Db.connect ~user ~password ~host ~port ~database () in
  Query.value dbh (<:value< currval $posts_id_seq$ >>)


let posts = (<:table< posts (
  id               bigint    NOT NULL DEFAULT(nextval $posts_id_seq$),
  user_id          bigint    NOT NULL,
  material_id      bigint    NOT NULL,
  exp              integer   NOT NULL,
  comments         text      NOT NULL,
  date_of_creation timestamp NOT NULL DEFAULT(current_timestamp)
) >>)

let get_post_by_id id : _ Lwt.t =
  Db.view
    (<:view< {
      p.id; p.user_id;
     } | p in $posts$; p.id = $int64:id$  >>)


let select_posts_of_user id : _ list Lwt.t =
  Db.view <:view< x order by x.date_of_creation desc | x in $posts$; x.user_id = $int64:id$ >>
  >|= (Core_list.map ~f:(fun x ->
                         object
                           method id          = x#!id
                           method material_id = x#!material_id
                           method exp      = x#!exp
                           method comments = x#!comments
                           method date_of_creation = x#!date_of_creation
                         end)
  )


let add_post ~userid ~text ~exp ~material_id =
  Db.query (<:insert< $posts$ := {
    id               = posts?id;
    user_id          = $int64:userid$;
    material_id      = $int64:material_id$;
    exp              = $int32:exp$;
    comments         = $string:text$;
    date_of_creation = posts?date_of_creation;
  } >>)

let skills_id_seq = (<:sequence< bigserial "skills_id_seq" >>)
let skills = (<:table< skills (
  id               bigint    NOT NULL DEFAULT(nextval $skills_id_seq$),
  descr            text      NOT NULL,
  maxexp           integer   NOT NULL
) >>)

let all_skills () =
  Db.view (<:view< { x.id; x.descr } | x in $skills$ >>)
  >|= (Core_list.map ~f:(fun o -> object method id = o#!id method descr = o#!descr end))

let skills_id_seq = (<:sequence< bigserial "parent_skills_id_seq" >>)
let parent_skills =  (<:table< parent_skills (
  child_id         bigint    NOT NULL DEFAULT(nextval $skills_id_seq$),
  parent_id        bigint
) >>)

let get_skill_links () =
  Db.view <:view< x | x in $parent_skills$ >>
  >|= (Core_list.map ~f:(fun o -> (o#!child_id, o#?parent_id)))

let materials_id_seq = (<:sequence< bigserial "materials_id_seq" >>)
let last_inserted_material_id (): int64 =
  print_endline "last_inserted_material_id";
  let open Db.DBSettings in
  let dbh = Query.Db.connect ~user ~password ~host ~port ~database () in
  Query.value dbh (<:value< currval $materials_id_seq$ >>)

let materials =  (<:table< materials (
  id               bigint    NOT NULL DEFAULT(nextval $materials_id_seq$),
  title            text      NOT NULL,
  author           text      NOT NULL,
  exp              integer   NOT NULL,
  profit           integer   NOT NULL,
  sort_id          bigint    NOT NULL,
  skill_id         bigint    NOT NULL
) >>)

let add_material ~title ~author ?(profit=Int32.zero) ?(sort_id=Int64.zero)  ?(exp=100l) ~skill_id =
 lwt () =
  Db.query (<:insert< $materials$ := {
    id               = materials?id;
    title            = $string:title$;
    author           = $string:author$;
    exp              = $int32:exp$;
    profit           = $int32:profit$;
    sort_id          = $int64:sort_id$;
    skill_id         = $int64:skill_id$
  } >>)
 in
 let open Db.DBSettings in
 let dbh = Query.Db.connect ~user ~password ~host ~port ~database () in
(* lwt dbh = Db.connect () in *)
 Query.value dbh (<:value< currval $materials_id_seq$ >>) |> Lwt.return

let find_material ~author ~title =
  Db.view_opt
    (<:view< {
      u.id;
      u.title;
      u.author;
     } | u in $materials$;
    u.title = $string:title$;
    u.author = $string:author$
    >>)
  >|= (function Some o -> Some (o#!id) | None -> None)
