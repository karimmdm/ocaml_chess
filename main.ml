open Graphics
open Gui
open State

(* (* [generate_board st pos] returns a new state with an updated board
   that reflects piece [p] moving to location [pos]. *) let
   generate_board st p pos = let board = State.board in board *)

(* [piece_selection st pos] validates whether or not [pos] is a valid
   location that contains a piece that the current player can move on
   their turn. *)
let piece_selection st pos =
  let p_turn = State.player_turn st in
  let clr = if p_turn = 1 then "white" else "black" in
  let p = Gui.get_piece st pos in
  match p with
  | None -> st
  | Some pce ->
      if Piece.color pce == clr then State.update_piece_clicked st p
      else st

(* [move_selection st pos] validates whether or not [pos] is a valid
   location that the current player can move the current piece selected
   to. *)
let move_selection st (pos : int * int) =
  let piece_clicked_op = State.piece_clicked st in
  match piece_clicked_op with
  | None -> st
  | Some piece_clicked -> Logic.move_piece st piece_clicked pos

(* [move st pos] checks the current state's piece_selected field to
   determine which phase of the turn the player is in: piece selection
   or move selection. *)
let move st pos =
  match State.piece_clicked st with
  | Some p -> move_selection st pos
  | None -> piece_selection st pos

let play_game (st : State.t) =
  try
    let my_player = 1 in
    let current_state = ref st in
    let game_running = ref true in
    let current_player = ref 1 in
    while !game_running do
      if !current_player = my_player then (
        let new_state = Gui.listen (move !current_state) in
        game_running := not (State.checkmate new_state);
        current_player := State.player_turn new_state;
        current_state := new_state;
        Gui.draw new_state)
      else ()
    done
  with Exit -> ()

let main () =
  let st = init_state () in
  Gui.init ();
  Gui.draw st;
  play_game st

let () = main ()
