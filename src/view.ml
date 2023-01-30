open Notty
open Notty_unix

let string_of_cell = function
  | Model.Empty -> "  "
  | Cactus -> "🌵"
  | Camel -> "🐫"
  | Snake -> "🐍"
  | Elephant -> "🐘"
  | Spider -> "🕷️"
  | Spider_egg -> "🕸️"

let draw_cell c = I.string A.empty (string_of_cell c)

let draw_world () =
  I.hcat @@ Array.to_list
  @@ Array.map
       (fun column -> I.vcat @@ Array.to_list @@ Array.map draw_cell column)
       Model.world

let terminal = Term.create ()
let render () = Term.image terminal (draw_world ())
