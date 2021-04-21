open Graphics
open Gui
open State

(* [play_game args] is a recursive function that keeps the program
   running. The user can input a "quit" (case-insensitive) command which
   will terminate the program and close the graphics window. As long as
   the program is running, the graphics window will still be open.
   [args] is not used. *)
let rec play_game args =
  print_endline "Enter quit to terminate the program: ";
  let input = read_line () in
  if String.equal (String.lowercase_ascii input) "quit" then ()
  else play_game args

let play_game' st =
  try
    while true do
      print_endline "listening for click...";
      let f pos =
        let piece_option = get_piece st pos in
        let piece_str =
          match piece_option with
          | None -> "Empty cell"
          | Some piece -> Printer.print_piece_type piece
        in
        print_endline piece_str
      in
      listen f;
      ()
    done
  with Exit -> ()

let main () =
  let st = init_state () in
  init ();
  draw st;
  play_game' st;
  play_game "dummy"

let () = main ()
