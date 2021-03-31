open Piece
open Graphics
open Images
open Graphic_image

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

let open_img path x y =
  let img = Png.load_as_rgb24 path [ Load_Resolution (x, y) ] in
  let img' = Graphic_image.of_image img in
  let to_transp ele = if ele = white then transp else ele in
  let color_arr =
    dump_image img' |> Array.map (fun x -> Array.map to_transp x)
  in
  make_image color_arr

let rec overlay_piece_img x y = function
  | [] -> ()
  | h :: t -> (
      match h with
      | None ->
          (* let img = Png.load_as_rgb24 "./images/wp.png" [
             Load_Resolution (80., 80.) ] *)
          (* let img = Images.load "./imagesjk255/WP.jpg" [
             Load_Resolution (80., 80.) ] in let img' =
             Graphic_image.of_image img in *)
          let img' = open_img "./images/br.png" 80. 80. in
          Graphics.draw_image img' ((x * 100) + 20) ((y * 100) + 20);
          let x' = if x = 7 then 0 else x + 1 in
          let y' = if x = 7 then y + 1 else y in
          overlay_piece_img x' y' t
      | Some piece ->
          let pos = position piece in
          let x = fst pos in
          let y = snd pos in
          ())

(* [gen_oriented_board_lst board player] gives the [board] as a
   flattened list oriented with respect to [player]*)
let gen_oriented_board_lst board player =
  let flattenedBoard = List.concat board in
  if player = 2 then List.rev flattenedBoard else flattenedBoard

let draw st =
  let board = State.board st in
  let player = State.player_turn st in
  let boardlst = gen_oriented_board_lst board player in
  clear_graph ();
  let grey = rgb 112 128 144 in
  set_color grey;
  gen_grid 0 0;
  overlay_piece_img 0 0 boardlst

let coordinate_pair status = (status.mouse_x / 100, status.mouse_y / 100)

let string_of_coordinate_pair tuple =
  string_of_int (fst tuple) ^ " " ^ string_of_int (snd tuple)

let listen f =
  let st = wait_next_event [ Button_down ] in
  if st.button then st |> coordinate_pair |> f
