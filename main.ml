open Graphics
open Gui
open State

(* [play_game args] is a recursive function that keeps the program
   running. The user can input a "quit" (case-insensitive) command which
   will terminate the program and close the graphics window. As long as
   the program is running, the graphics window will still be open.
   [args] is not used. *)

(* let play_game st args = try while true do print_endline "listening
   for click..."; let f (x, y) = highlight_squares st (x, y) (piece st
   (x, y)) in (* let f (x, y) = print_endline (string_of_coordinate_pair
   (x, y)) in *) listen f done *)

let move st pos = 
  match State.piece_clicked st with
  | Some p ->
  let piece_pos = Piece.position p in
  let valid_locs = State.locations in st
  | None -> let pc = get_piece st pos in
  match pc with
  | Some p -> State.update_piece_clicked st pc
  | None -> st 

let play_game st =
  try
    while true do
      print_endline "listening for click...";
      (* let f pos = move st pos in *)
      let f pos = highlight_squares st pos (get_piece st pos) in
      listen f
    done
  with Exit -> ()

let main () =
  let st = init_state () in
  init ();
  draw st;
  play_game st

let () = main ()
