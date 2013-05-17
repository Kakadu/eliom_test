open Printf
module Option = Eliom_lib.Option

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

type password = Bcrypt.hash_t
(*
let to_password x = Bcrypt.hash x
let check_password = Bcrypt.verify
  *)

let users_id_seq = (<:sequence< bigserial "users_id_seq" >>)

let users = (<:table< users (
  id              bigint NOT NULL DEFAULT(nextval $users_id_seq$),
  nick            text NOT NULL,
  email           text      NOT NULL,
  password_digest text      NOT NULL,
  exp             integer   NOT NULL
) >>)

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

let get_user_by_name name =
  Db.view_opt
    (<:view< {
      u.id;
      u.nick;
      u.password_digest;
      u.email;
     } | u in $users$;
    u.nick = $string:name$;
    >>)
  >>= fun user ->
  Lwt.return
    (Option.map
       (fun x ->
        object
          method id = x#id;
          method name = x#nick;
          method password = x#password_digest;
        end
       )
       user
    )

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

let check_password nick (password: string) =
  Db.view_opt
    (<:view< {
      u.password_digest;
      u.nick;
     } | u in $users$;
    u.nick = $string:nick$;
    u.password_digest = $string:password$
    >>)
  >|= (function Some _ -> true | None -> false)

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
