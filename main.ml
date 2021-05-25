let get_user_input () =
  match read_line () with
  | exception End_of_file -> failwith "Invalid input"
  | user_input -> user_input

let create_game () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the OCaml Multiplayer Chess Game!\n";
  print_endline "Please enter a room name.\n>";
  let room_id = get_user_input () in
  let state_fen = Client.create_room_request room_id in
  Game.play_game
    (State.state_from_fen state_fen None)
    Game.Create (Some room_id)

let join_game () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the OCaml Multiplayer Chess Game!\n";
  print_endline "Please enter a valid room name to join a game.\n> ";
  let room_id = get_user_input () in
  let state_fen = Client.get_state_by_id_request room_id in
  Game.play_game
    (State.state_from_fen state_fen None)
    Game.Join (Some room_id)

let title =
  Interactive.make_text (160, 600) 200 0 "Chess" (fun () -> ())

let create_game_text =
  Interactive.make_text (10, 200) 50 264 "Create Game" (fun () ->
      create_game ())

let join_game_text =
  Interactive.make_text (10, 100) 50 216 "Join Game" (fun () ->
      join_game ())

let local_game_text =
  Interactive.make_text (10, 0) 50 240 "Local Game" (fun () ->
      Game.play_game (State.init_state ()) Game.Local None)

let on_menu_click clickables click_pos =
  List.iter
    (fun c ->
      Interactive.on_click click_pos c
        (Interactive.get_clickable_function c))
    clickables;
  State.init_state ()

let main () =
  try
    Gui.init ();
    let clickables_lst =
      [ create_game_text; join_game_text; local_game_text; title ]
    in
    Gui.draw_interactives clickables_lst;
    while true do
      ignore (Gui.listen false (on_menu_click clickables_lst))
    done
  with
  | Graphics.Graphic_failure n -> exit 0
  | Failure f ->
      print_endline f;
      exit 1

let () = main ()
