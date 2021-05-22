let rec on_menu_click pos =
  print_endline (Gui.string_of_coordinate_pair pos);
  State.init_state ()

let main () =
  let create_game = Clickables.Text ((10, 200), 50, "Create Game") in
  let join_game = Clickables.Text ((10, 100), 50, "Join Game") in
  let local_game = Clickables.Text ((10, 0), 50, "Local Game") in
  Gui.init ();
  Gui.draw_start_screen () [ create_game; join_game; local_game ];
  while true do
    Gui.listen false on_menu_click
  done

let () = main ()
