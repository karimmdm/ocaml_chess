open Graphics
open Gui
open State
open Logic

(* [deselect st] resets the player's turn to the piece selection phase
   by resetting the current state's piece selected to None. *)
let deselect st = State.update_piece_clicked st None

(* [piece_selection st pos] validates whether or not [pos] is a valid
   location that contains a piece that the current player can move on
   their turn. *)
let piece_selection st pos =
  print_endline "Piece selection";
  let p_turn = State.player_turn st in
  let clr = if p_turn = 1 then "white" else "black" in
  let p = Gui.get_piece st pos in
  match p with
  | None -> st
  | Some pce ->
      if Piece.color pce <> clr then st
      else State.update_piece_clicked st p

(* [move_selection st pos] validates whether or not [pos] is a valid
   location that the current player can move the current piece selected
   to. *)
let move_selection st pos =
  print_endline "Move selection";
  match State.piece_clicked st with
  | None ->
      failwith
        "Move selection shouldn't be called when piece_selected is None"
  | Some p ->
      if Logic.valid_move st p pos then Logic.move_piece st p pos
      else deselect st

(* [move my_player pos] checks the current state's piece_selected field
   to determine which phase of the turn the player is in: piece
   selection or move selection. *)
let move st my_player pos =
  let pos = Gui.invert_pos my_player pos in
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
          else move_selection st pos)

let play_game () =
  try
    let my_player = 1 in
    let st = State.init_state () in
    Gui.init ();
    let img_dict = Gui.images_dict st in
    Gui.draw st my_player img_dict;
    let current_state = ref st in
    let game_running = ref true in
    let current_player = ref 1 in
    while !game_running do
      (* print_endline "Loop start"; *)
      (* if !current_player = my_player then ( *)
      (* !cp = !cp for testing purposes only *)
      if !current_player = !current_player then (
        print_endline ("Player turn " ^ string_of_int !current_player);
        let new_state = Gui.listen (move !current_state my_player) in
        game_running :=
          not (State.checkmate new_state || State.stalemate new_state);
        (* print_endline ("Current state checkmate: " ^ string_of_bool
           (State.checkmate new_state)); *)
        current_player := State.player_turn new_state;
        current_state := new_state;
        Gui.draw !current_state my_player img_dict)
      else print_endline "Game Over";
      ()
    done
  with Exit -> ()

let main () = play_game ()

let () = main ()
