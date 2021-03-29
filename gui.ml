open Graphics

let init () =
  open_graph " 800x800";
  set_window_title "Chess";
  ()

(* [gen_grid x y pieces] draws either a black or white square starting
   at ([x],[y]). If there is a piece at that location according to
   [pieces] then the image of the piece is drawn over the square *)
let rec gen_grid x y pieces =
  let rec gen_grid_horizontal x y =
    if x >= 800 then ()
    else (
      if (x + y) mod 200 == 0 then fill_rect x y 100 100;
      gen_grid_horizontal (x + 100) y)
  in
  if y >= 800 then () else gen_grid_horizontal x y;
  gen_grid x (y + 100) pieces

let draw st = failwith "Unimplemented"

let coordinate_pair status = (status.mouse_x, status.mouse_y)

let string_of_coordinate_pair tuple =
  string_of_int (fst tuple / 100) ^ " " ^ string_of_int (snd tuple / 100)

let listen f =
  let st = wait_next_event [ Button_down ] in
  if st.button then st |> coordinate_pair |> f
