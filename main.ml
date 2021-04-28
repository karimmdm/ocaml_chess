open Graphics
open Gui
open State
open Logic

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
      else
        (* Gui.highlight_valid_locations st p; *)
        State.update_piece_clicked st p

(* [move_selection st pos] validates whether or not [pos] is a valid
   location that the current player can move the current piece selected
   to. *)
let move_selection st pos =
  print_endline "Move selection";
  let piece_clicked_op = State.piece_clicked st in
  match piece_clicked_op with
  | None -> st
  | Some piece_clicked -> Logic.move_piece st piece_clicked pos

(* [move st pos] checks the current state's piece_selected field to
   determine which phase of the turn the player is in: piece selection
   or move selection. *)
let move st pos =
  match State.piece_clicked st with
  | None -> piece_selection st pos
  | Some p -> (
      let new_piece = Gui.get_piece st pos in
      match new_piece with
      | None -> move_selection st pos
      | Some pce ->
          let p_turn = State.player_turn st in
          let clr = Piece.color pce in
          if
            (p_turn == 1 && String.equal clr "white")
            || (p_turn == 2 && String.equal clr "black")
          then piece_selection st pos
          else move_selection st pos)

let play_game st =
  try
    let my_player = 1 in
    let current_state = ref st in
    let game_running = ref true in
    let current_player = ref 1 in
    while !game_running do
      print_endline "Loop running";
      if !current_player = my_player then (
        print_endline ("Player turn " ^ string_of_int !current_player);
        let new_state = Gui.listen (move !current_state) in
        (* print_endline (Printer.print_board new_state); *)
        game_running := not (State.checkmate new_state);
        current_player := State.player_turn new_state;
        current_state := new_state;
        print_endline
          ("Piece clicked: "
          ^ Printer.print_piece_option
              (State.piece_clicked !current_state));
        Gui.draw new_state)
      else ()
    done
  with Exit -> ()

let play_game' st =
  try
    while true do
      print_endline "Listening for click...";
      let f pos = move st pos in
      listen f
    done
  with Exit -> ()

let main () =
  let st = init_state () in
  Gui.init ();
  Gui.draw st;
  play_game st

let () = main ()
