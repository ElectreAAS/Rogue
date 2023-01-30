type cell = Empty | Cactus | Camel | Snake | Elephant | Spider | Spider_egg
type world = cell array array

let width, height = (50, 30)
let world = Array.make_matrix width height Empty
let get (x, y) = try world.(x).(y) with Invalid_argument _ -> Cactus
let set (x, y) v = world.(x).(y) <- v
let () = Random.self_init ()
let random_position () = (Random.int width, Random.int height)

let () =
  for _ = 0 to 200 do
    set (random_position ()) Cactus
  done;
  for _ = 0 to 20 do
    set (random_position ()) Snake
  done;
  for _ = 0 to 10 do
    set (random_position ()) Elephant
  done;
  for _ = 0 to 3 do
    set (random_position ()) Spider
  done

let camel_init = random_position ()
let () = set camel_init Camel
let () = print_endline "Hello world"
