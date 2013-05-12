open Eliom_content.Html5.D
open Eliom_parameter

(* Adopted from https://github.com/chuajiesheng/eliom-projects/blob/master/db-app/dbapp.eliom *)
exception E of string

let start =
  (* The database is called test.db. Delete it if it already exists. *)
  let db_filename = "users.db" in
  (    try Unix.unlink db_filename
       with _ -> ()
  ) ;

  (* Create a new database. *)
  let db = Sqlite3.db_open db_filename in

  (* Close database when done. *)
  if Sqlite3.db_close db then print_endline "All done.\n"
  else print_endline "Cannot close database.\n"

let create_tables db =
  (* Create two tables in the database. *)
  let tables =
    [    "users", "user_id INTEGER PRIMARY KEY, username TEXT, password TEXT" ;
         "properties", "property_id INTEGER PRIMARY KEY, property TEXT, value TEXT" ;
    ]
  in
  let make_table (name, layout) =
    let stmt = Printf.sprintf "CREATE TABLE %s (%s);" name layout in
    match Sqlite3.exec db stmt with
    |    Sqlite3.Rc.OK -> Printf.printf "Table '%s' created.\n" name
    |    x -> raise (E (Sqlite3.Rc.to_string x))
  in
  List.iter make_table tables

let _ =
  Eliom_registration.Html5.register_service
    ~path:["create"]
    ~get_params:(Eliom_parameter.unit)
    (fun () () ->
      start;

      let db = Sqlite3.db_open "users.db" in
      create_tables db;
      let (_:bool) = Sqlite3.db_close db in
      Lwt.return
        Eliom_content.Html5.D.(html
                   (head (title(pcdata "DB Creation")) [])
                   (body [p [ pcdata "DB created";
                            ]])))
