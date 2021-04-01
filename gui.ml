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
          let symbol = print_piece_type piece in
          moveto ((x * 100) + 20) ((y * 100) + 20);
          Graphics.draw_string symbol;
          overlay_piece_icon t)

let rec string_flattened = function
  | [] -> "]"
  | h :: t -> (
      match h with
      | None -> "empty; " ^ string_flattened t
      | Some p ->
          Printer.print_piece p ^ ": "
          ^ Printer.print_piece_position p
          ^ "; " ^ string_flattened t)

(* [gen_board_lst board player] gives the [board] as a flattened list*)
let gen_board_lst board =
  let flattenedBoard = List.concat board in
  flattenedBoard

let draw st =
  print_endline (Printer.print_board st);
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

let rec listen (f : int * int -> unit) =
  let st = wait_next_event [ Button_down ] in
  if st.button then st |> coordinate_pair |> f else listen f
