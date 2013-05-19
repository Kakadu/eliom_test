open Printf
module Option = Eliom_lib.Option

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

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
