let get_user_input () =
  match read_line () with
  | exception End_of_file -> failwith "Invalid input"
  | user_input -> user_input

let get_state_by_id_request room_id =
  let url = "localhost:3000/get/" ^ room_id in
  match Curly.(run (Request.make ~url ~meth:`GET ())) with
  | Ok x -> x.Curly.Response.body
  | Error e -> failwith ("error finding a room name: " ^ room_id)

let join_game () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the OCaml Multiplayer Chess Game!\n";
  print_endline
    "Please enter a valid room name to join the game as Black.\n> ";
  let room_id = get_user_input () in
  let state_fen = get_state_by_id_request room_id in
  Game.play_game (State.state_from_fen state_fen None)

let create_room_request room_id =
  let url = "localhost:3000/post/rooms" in
  let body =
    {|{"room_id": |} ^ "\"" ^ room_id ^ "\"" ^ {|, "state_fen": |}
    ^ "\"" ^ State.init_fen () ^ "\"}"
  in
  match Curly.(run (Request.make ~body ~url ~meth:`POST ())) with
  | Ok x -> x.Curly.Response.body
  | Error e -> failwith "error making a room"

let create_game () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the OCaml Multiplayer Chess Game!\n";
  print_endline "Please enter a room name.\n>";
  let room_id = get_user_input () in
  let state_fen = create_room_request room_id in
  print_endline state_fen;
  Game.play_game (State.state_from_fen state_fen None)
