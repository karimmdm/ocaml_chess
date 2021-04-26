open Piece
open Graphics
open Images
open Graphic_image
open Printer

let init () =
  open_graph " 800x800";
  set_window_title "Chess";
  ()

(* [gen_grid_horizontal] x y draws horizonally *)
let rec gen_grid_horizontal x y =
  if x >= 800 then ()
  else (
    if (x + y) mod 200 == 0 then fill_rect x y 100 100;
    gen_grid_horizontal (x + 100) y)

(* [gen_grid x y pieces] draws either a black or white square starting
   at ([x],[y]). If there is a piece at that location according to
   [pieces] then the image of the piece is drawn over the square *)
let rec gen_grid x y =
  if y >= 800 then ()
  else (
    gen_grid_horizontal x y;
    gen_grid x (y + 100))

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

(* [overlay_piece_icon lst] draws the image of the piece *)
let rec overlay_piece_img player = function
  | [] -> ()
  | h :: t -> (
      match h with
      | None -> overlay_piece_img player t
      | Some piece ->
          let pos = position piece in
          let y = if player = 1 then 7 - fst pos else fst pos in
          let x = snd pos in
          let img = open_img (icon piece) 80. 80. in
          Graphics.draw_image img ((x * 100) + 20) ((y * 100) + 20);
          overlay_piece_img player t)

(* [overlay_piece_icon lst] draws the letter of the piece *)
let rec overlay_piece_icon = function
  | [] -> ()
  | h :: t -> (
      match h with
      | None -> overlay_piece_icon t
      | Some piece ->
          let pos = position piece in
          let y = fst pos in
          let x = snd pos in
          let symbol = Printer.print_piece_type piece in
          moveto ((x * 100) + 20) ((y * 100) + 20);
          Graphics.draw_string symbol;
          overlay_piece_icon t)

(* [gen_board_lst board player] gives the [board] as a flattened list*)
let gen_board_lst board =
  let flattenedBoard = List.concat board in
  flattenedBoard

let draw st =
  let board = State.board st in
  let player = State.player_turn st in
  let boardlst = gen_board_lst board in
  clear_graph ();
  let grey = rgb 112 128 144 in
  set_color grey;
  gen_grid 0 0;
  set_text_size 50;
  overlay_piece_img player boardlst

let coordinate_pair status = (status.mouse_x / 100, status.mouse_y / 100)

let string_of_coordinate_pair tuple =
  string_of_int (fst tuple) ^ " " ^ string_of_int (snd tuple)

let get_piece st ((x, y) : int * int) =
  let y' = if State.player_turn st = 1 then 7 - y else y in
  let pos = (y', x) in
  let rec helper = function
    | [] -> None
    | h :: t -> (
        match h with
        | None -> helper t
        | Some piece ->
            if Piece.position piece = pos then h else helper t)
  in
  helper (gen_board_lst (State.board st))

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

let highlight_valid_locations st p_op =
  match p_op with
  | None -> ()
  | Some piece ->
      (* clear the board before highlighting again *)
      draw st;
      (* highlight the possible locations *)
      let locations = State.locations st piece in
      let locations_cartesian =
        List.map
          (fun (row, col) ->
            let x = col in
            let y = if State.player_turn st = 1 then 7 - row else row in
            (x * 100, y * 100))
          locations
      in
      List.iter (draw_border green) locations_cartesian;
      (* draw a boarder around the clicked piece *)
      let pos = Piece.position piece in
      let x = snd pos in
      let y =
        if State.player_turn st = 1 then 7 - fst pos else fst pos
      in
      draw_border blue (x * 100, y * 100)

let rec listen (f : int * int -> unit) =
  let st = wait_next_event [ Button_down ] in
  if st.button then st |> coordinate_pair |> f else listen f
