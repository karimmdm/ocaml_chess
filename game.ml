type mode =
  | Create
  | Join
  | Local

(**[deselect st] resets the player's turn to the piece selection phase
   by resetting the current state's piece selected to None. *)
let deselect st = State.update_piece_clicked st None

(**[piece_selection st pos] validates whether or not [pos] is a valid
   location that contains a piece that the current player can move on
   their turn. *)
let piece_selection st pos =
  (**print_endline "Piece selection"; *)
  let p_turn = State.player_turn st in
  let clr = if p_turn = 1 then "white" else "black" in
  let p = Gui.get_piece st pos in
  match p with
  | None -> st
  | Some pce ->
      if Piece.color pce <> clr then st
      else State.update_piece_clicked st p

(**[move_selection st pos] validates whether or not [pos] is a valid
   location that the current player can move the current piece selected
   to. *)
let move_selection st pos =
  (**print_endline "Move selection"; *)
  match State.piece_clicked st with
  | None ->
      failwith
        "Move selection shouldn't be called when piece_selected is None"
  | Some p ->
      if Logic.valid_move st p pos then Logic.move_piece st p pos
      else deselect st

(**[state_after_move st player pos] is the state after a checks the
   current state's piece_selected field to determine which phase of the
   turn the player is in: piece selection or move selection. *)
let state_after_move st player pos =
  let pos = Gui.invert_pos player pos in
  match State.piece_clicked st with
  | None -> piece_selection st pos
  | Some p -> (
      let user_clicked = Gui.get_piece st pos in
      match user_clicked with
      | None -> move_selection st pos
      | Some pce ->
          let p_turn = State.player_turn st in
          let clr = Piece.color pce in
          if
            (p_turn == 1 && String.equal clr "white")
            || (p_turn == 2 && String.equal clr "black")
          then piece_selection st pos
          else move_selection st pos )

(**[move md player pos] moves the [player] to [pos] and updates the
   state locally or on server based on the mode [md] *)
let move md room_id st player pos =
  let st' = state_after_move st player pos in
  let st'_fen = State.to_fen st' in
  match md with Local -> st' | _ -> st'

(**if st'_fen <> State.to_fen st then let get_st =
   Client.update_room_state room_id st'_fen in State.state_from_fen
   get_st None else st' *)

(**[game_loop md current_player my_player room game_running
   current_state img_dict room_text] is the game loop helper for
   play_game()*)
let game_loop
    md
    current_player
    my_player
    room
    game_running
    current_state
    img_dict
    room_text =
  let conditional =
    match md with
    | Local -> !current_player = !current_player
    | _ -> !current_player = !my_player
  in
  if conditional then (
    let new_state =
      Gui.listen true (move md room !current_state !current_player)
    in
    game_running :=
      not (State.checkmate new_state || State.stalemate new_state);
    current_player := State.player_turn new_state;
    my_player := if md = Local then !current_player else !my_player;
    current_state := new_state;
    Gui.draw_game !current_state !my_player img_dict room_text )
  else ()

let play_game st md room_id =
  let current_state = ref st in
  let current_player = ref (State.player_turn !current_state) in
  let my_player =
    match md with
    | Create -> ref 1
    | Join -> ref !current_player
    | Local -> ref 1
  in
  let room =
    match room_id with Some rm -> rm | None -> "Local Room"
  in
  let img_dict = Gui.images_dict st in
  let room_interactive_text =
    Interactive.make_text (10, 810) 20 0 ("Room name: " ^ room)
      (fun _ -> ())
  in
  Gui.draw_game st !my_player img_dict room_interactive_text;
  let game_running = ref true in
  while !game_running do
    game_loop md current_player my_player room game_running
      current_state img_dict room_interactive_text
  done;
  let winner = if !current_player = 1 then "Black" else "White" in
  let game_over_text =
    Interactive.make_text (300, 400) 200 0
      ("Game Over: " ^ winner ^ " is the winner")
      (fun _ -> ())
  in
  Gui.draw_interactives [ game_over_text ]
