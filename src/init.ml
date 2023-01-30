open Model

let fill_world () =
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
  done;
  set (random_position ()) Camel

let go () =
  Random.self_init ();
  fill_world ()
