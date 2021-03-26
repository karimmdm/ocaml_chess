open Graphics

(* [play_game args] is a recursive function that keeps the program running
  unless the user inputs a "quit" (case-insensitive) command which will
  terminate the program and close the graphics window. As long as the program
  is running, the graphics window will still be open.  *)
let rec play_game args =
  print_endline "Enter quit terminate the program: ";
  let input = read_line () in
  if String.equal (String.lowercase_ascii input) "quit" then
    ()
  else 
    play_game args

let main () =
  open_graph " 800x800";
  set_window_title "Chess";
  play_game "dummy";
  ()

let () = main ()
