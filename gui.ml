open Graphics

(* [draw_clickable_text x y] draws a rectangle button of color [color]
   of size [w] * [h] at coordinate location ([x], [y]) with text [text]
   at the center*)
let draw_clickable_text color (x, y) font_size text =
  moveto x y;
  set_color color;
  set_text_size font_size;
  draw_string text

let draw_start_screen () clickable_lst =
  let rec helper = function
    | h :: t ->
        draw_clickable_text black
          (Clickables.get_clickable_pos h)
          (Clickables.get_clickable_font_size h)
          (Clickables.get_clickable_text h);
        helper t
    | [] -> ()
  in
  helper clickable_lst

let init () =
  open_graph " 800x800";
  set_window_title "Chess";
  ()

let invert_pos my_player (x, y) =
  let y' = if my_player = 1 then 7 - y else y in
  (y', x)

(* [gen_grid_horizontal x y] draws a specific row *)
let rec gen_grid_horizontal x y my_player =
  if x >= 800 then ()
  else if my_player = 1 then (
    if (x + y) mod 200 = 0 then fill_rect x y 100 100;
    gen_grid_horizontal (x + 100) y my_player)
  else (
    if (x + y) mod 200 <> 0 then fill_rect x y 100 100;
    gen_grid_horizontal (x + 100) y my_player)

(* [gen_grid x y pieces] draws either a black or white square starting
   at ([x],[y]). If there is a piece at that location according to
   [pieces] then the image of the piece is drawn over the square *)
let rec gen_grid x y my_player =
  let grey = rgb 112 128 144 in
  set_color grey;
  if y >= 800 then ()
  else
    (gen_grid_horizontal x y my_player;
     gen_grid x (y + 100))
      my_player

(* [open_img path x y] is the image found at [path] with resolution
   ([x], [y])*)
let open_img path x y =
  let img = Png.load_as_rgb24 path [ Load_Resolution (x, y) ] in
  let img' = Graphic_image.of_image img in
  let to_transp ele = if ele = white then transp else ele in
  let color_arr =
    dump_image img' |> Array.map (fun x -> Array.map to_transp x)
  in
  make_image color_arr

let images_dict st =
  let board = State.board st in
  let tbl = Hashtbl.create 32 in
  let add_to_board = function
    | None -> ()
    | Some p ->
        let path = Piece.icon p in
        Hashtbl.add tbl path (open_img path 80. 80.)
  in
  List.iter add_to_board (State.gen_falttened_board board);
  tbl

let coordinate_pair_bound status =
  (status.mouse_x / 100, status.mouse_y / 100)

let coordinate_pair status = (status.mouse_x, status.mouse_y)

let string_of_coordinate_pair tuple =
  string_of_int (fst tuple) ^ " " ^ string_of_int (snd tuple)

let get_piece st ((x, y) : int * int) =
  let rec helper = function
    | [] -> None
    | h :: t -> (
        match h with
        | None -> helper t
        | Some piece ->
            if Piece.position piece = (x, y) then h else helper t)
  in
  helper (State.gen_falttened_board (State.board st))

let highlight_square clr loc =
  set_color black;
  fill_rect (fst loc) (snd loc) 100 100;
  set_color clr;
  fill_rect (fst loc + 5) (snd loc + 5) 90 90

let draw_border clr (x, y) =
  set_color clr;
  set_line_width 2;
  moveto (x + 1) y;
  lineto (current_x () + 99) (current_y ());
  lineto (current_x ()) (current_y () + 99);
  lineto (current_x () - 99) (current_y ());
  lineto (current_x ()) (current_y () - 99)

let highlight_valid_locations st p_op my_player =
  match p_op with
  | None -> ()
  | Some piece ->
      (* clear the board before highlighting again *)
      (* highlight the possible locations *)
      let locations = Logic.locations st piece in
      let locations_cartesian =
        List.map
          (fun (row, col) ->
            let x = col in
            let y = if my_player = 1 then 7 - row else row in
            (x * 100, y * 100))
          locations
      in
      List.iter (draw_border green) locations_cartesian;
      (* draw a border around the clicked piece *)
      let pos = Piece.position piece in
      let x = snd pos in
      let y = if my_player = 1 then 7 - fst pos else fst pos in
      draw_border blue (x * 100, y * 100)

let draw_game st my_player img_tbl =
  let board = State.board st in
  let boardlst = State.gen_falttened_board board in
  clear_graph ();
  gen_grid 0 0 my_player;
  set_text_size 50;
  let rec overlay_piece_img my_player = function
    | [] -> ()
    | h :: t -> (
        match h with
        | None -> overlay_piece_img my_player t
        | Some piece ->
            let pos = Piece.position piece in
            let y = if my_player = 1 then 7 - fst pos else fst pos in
            let x = snd pos in
            let img = Hashtbl.find img_tbl (Piece.icon piece) in
            draw_image img ((x * 100) + 20) ((y * 100) + 20);
            overlay_piece_img my_player t)
  in
  overlay_piece_img my_player boardlst;
  highlight_valid_locations st (State.piece_clicked st) my_player

let rec listen bound (f : int * int -> State.t) =
  let st = wait_next_event [ Button_down ] in
  if st.button then
    st
    |> (if bound then coordinate_pair_bound else coordinate_pair)
    |> f
  else listen bound f
