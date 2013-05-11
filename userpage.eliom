open Eliom_content.Html5.D
open Eliom_parameter



let page ~name =
  [ div [pcdata "userpage"]
  ; div [h1 [pcdata name]]
  ; div [b [pcdata "Content column"; em [pcdata ""]]]
  ; div [pcdata "some text some text some text some text some text some text some text some text some text some text some text some text some text some text some text some text some text some text some text some text some text some text some text some text some text "]
  ]
