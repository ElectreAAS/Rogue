open Model
open View
open Notty_unix

let keyboard_dir () =
  match Term.event terminal with
  | `Key (`Escape, _) | `Key (`ASCII 'q', _) -> exit 0
  | `Key (`Arrow `Left, _) -> (-1, 0)
  | `Key (`Arrow `Right, _) -> (1, 0)
  | `Key (`Arrow `Up, _) -> (0, -1)
  | `Key (`Arrow `Down, _) -> (0, 1)
  | _ -> (0, 0)

let rec camel current_pos =
  let new_pos = current_pos ++ keyboard_dir () in
  let pos = move current_pos new_pos in
  Effect.perform End_of_turn;
  camel pos
