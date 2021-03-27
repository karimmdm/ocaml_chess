open Graphics

(* [play_game args] is a recursive function that keeps the program
   running unless the user inputs a "quit" (case-insensitive) command
   which will terminate the program and close the graphics window. As
   long as the program is running, the graphics window will still be
   open. *)
let rec play_game args =
  print_endline "Enter quit terminate the program: ";
  let input = read_line () in
  if String.equal (String.lowercase_ascii input) "quit" then ()
  else play_game args

let rec gen_grid_horizontal x y =
  if x >= 800 then ()
  else (
    if (x + y) mod 200 == 0 then fill_rect x y 100 100;
    gen_grid_horizontal (x + 100) y;
    ())

let rec gen_grid x y =
  if y >= 800 then ()
  else (
    gen_grid_horizontal x y;
    gen_grid x (y + 100))

let main () =
  open_graph " 800x800";
  set_window_title "Chess";
  set_color black;
  gen_grid 0 0;
  play_game "dummy";
  ()

let () = main ()
