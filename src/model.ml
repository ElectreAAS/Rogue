open Effect.Deep

type cell = Empty | Cactus | Camel | Snake | Elephant | Spider | Spider_egg
type world = cell array array

let width, height = (50, 30)
let world = Array.make_matrix width height Empty
let get (x, y) = try Some world.(x).(y) with Invalid_argument _ -> None
let set (x, y) v = try world.(x).(y) <- v with Invalid_argument _ -> ()
let random_position () = (Random.int width, Random.int height)
let ( ++ ) (x, y) (dx, dy) = (x + dx, y + dy)

let move old_pos new_pos =
  match (get new_pos, get old_pos) with
  | Some Empty, Some actor ->
      set old_pos Empty;
      set new_pos actor;
      new_pos
  | _ -> old_pos

type _ Effect.t += End_of_turn : unit Effect.t

(** Wait for n turns *)
let rec wait = function
  | 0 -> ()
  | n ->
      Effect.perform End_of_turn;
      wait (n - 1)

let queue : (unit -> unit) Queue.t = Queue.create ()

let player character =
  match_with character ()
    {
      effc =
        (fun (type c) (eff : c Effect.t) ->
          match eff with
          | End_of_turn ->
              Some
                (fun (k : (c, _) continuation) -> Queue.add (continue k) queue)
          | _ -> None);
      exnc = raise;
      retc = Fun.id;
    }

let spawn child = Queue.add (fun () -> player child) queue
