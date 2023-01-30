open Model

(*****************************************************************************)
(* Common behaviour *)

let all_dirs = [ (1, 0); (-1, 0); (0, 1); (0, -1) ]
let random_dir () = List.nth all_dirs (Random.int (List.length all_dirs))

let random_move old_pos =
  let new_pos = move old_pos (old_pos ++ random_dir ()) in
  Effect.perform End_of_turn;
  new_pos

(*****************************************************************************)
(* Snake: it just moves in a random direction. *)
(* If that move is impossible the snake just stays put. *)

let rec snake pos = snake (random_move pos)

(*****************************************************************************)
(* Elephant: if it can see the camel it will charge at it,
   otherwise move randomly. *)

let camel_in_sight pos =
  let rec aux pos dir =
    let next_pos = pos ++ dir in
    match get next_pos with
    | Some Camel -> true
    | Some Empty -> aux next_pos dir
    | _ -> false
  in
  List.find_opt (aux pos) all_dirs

let rec charge pos dir =
  let next_pos = pos ++ dir in
  match get next_pos with
  | Some Empty ->
      let next_pos = move pos next_pos in
      Effect.perform End_of_turn;
      charge next_pos dir
  | Some Cactus | None ->
      (* Knocked out! *)
      wait 20;
      pos
  | _ ->
      Effect.perform End_of_turn;
      pos

let rec elephant pos =
  match camel_in_sight pos with
  | None -> elephant (random_move pos)
  | Some dir -> elephant (charge pos dir)

(*****************************************************************************)
(* Spider: it tries to lay eggs around it, and moves randomly. *)
(* Spider egg: wait 10 turns to hatch, then spawns spiders all around itself.*)
let rec spider pos =
  try_to_lay_egg pos;
  spider (random_move pos)

and try_to_lay_egg pos =
  let egg_pos = pos ++ random_dir () in
  if get egg_pos = Some Empty && Random.int 100 = 0 then (
    set egg_pos Spider_egg;
    spawn (fun () -> egg egg_pos))

and egg pos =
  wait 10;
  for dx = -1 to 1 do
    for dy = -1 to 1 do
      let child_pos = pos ++ (dx, dy) in
      if get child_pos = Some Empty then (
        set child_pos Spider;
        spawn (fun () -> spider child_pos))
    done
  done
