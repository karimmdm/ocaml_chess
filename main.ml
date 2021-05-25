let title =
  Interactive.make_text (160, 600) 200 0 "Chess" (fun () -> ())

let create_game =
  Interactive.make_text (10, 200) 50 264 "Create Game" (fun () ->
      Client.create_game ())

let join_game =
  Interactive.make_text (10, 100) 50 216 "Join Game" (fun () ->
      Client.join_game ())

let local_game =
  Interactive.make_text (10, 0) 50 240 "Local Game" (fun () ->
      Game.play_game (State.init_state ()) Game.Local)

let on_menu_click clickables click_pos =
  List.iter
    (fun c ->
      Interactive.on_click click_pos c
        (Interactive.get_clickable_function c))
    clickables;
  State.init_state ()

let main () =
  Gui.init ();
  let clickables_lst = [ create_game; join_game; local_game; title ] in
  Gui.draw_start_screen () clickables_lst;
  while true do
    ignore (Gui.listen false (on_menu_click clickables_lst))
  done

let () = main ()
