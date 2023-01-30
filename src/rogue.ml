open Model

let rec main_loop () =
  View.render ();
  let next_to_move = Queue.pop queue in
  next_to_move ();
  main_loop ()

let () =
  Init.go ();
  (world
  |> Array.iteri @@ fun x ->
     Array.iteri @@ fun y -> function
     | Camel -> spawn @@ fun () -> Controls.camel (x, y)
     | Spider -> spawn @@ fun () -> Fauna.spider (x, y)
     | Elephant -> spawn @@ fun () -> Fauna.elephant (x, y)
     | Snake -> spawn @@ fun () -> Fauna.snake (x, y) | _ -> ());
  main_loop ()
