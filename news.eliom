open All_services
open Eliom_content.Html5.D
open Core_kernel.Std

let template o = Userpage.posts_content o

let _ =
  let open Core_kernel.Std in
  WithDefault.register ~service:news_service (fun  () () -> Lwt.return (fun (_,id) ->
    lwt posts = Db_user.select_news_for_user id in
    lwt divs = Lwt_list.map_p template posts in

    Main.wrap_main_page
      [ div [pcdata "news"
            ; br ()
            ; div divs]
      ] |> Lwt.return (*
    Lwt.return
            (html
               (head (title (pcdata ""))
                  [ css_link ~uri:(make_uri (Eliom_service.static_dir ()) ["admin.css"]) ()
                  ])
               (body [p [pcdata "news"]; div []
                     ])
            )
                                                      *)
  ))
