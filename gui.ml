open Graphics
open Png
open Graphic_image
open Piece

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

let string_of_pair x y =
  "( " ^ string_of_int x ^ ", " ^ string_of_int y ^ " )"

let string_of_piece_option_pos = function
  | None -> ""
  | Some p -> string_of_pair (position p |> fst) (position p |> snd)

let string_piece_list l =
  List.fold_right
    (fun x acc -> string_of_piece_option_pos x ^ ", " ^ acc)
    l ""

let print_ll l =
  List.iter (fun ll -> print_endline (string_piece_list ll)) l

let string_board bd =
  let res = ref "" in
  for i = 0 to Array.length bd - 1 do
    for j = 0 to Array.length bd.(i) - 1 do
      match bd.(i).(j) with
      | None -> res := !res ^ "empty; "
      | Some p ->
          let x = fst (position p) in
          let y = snd (position p) in
          res := !res ^ string_of_pair x y ^ "; "
      (* let img = Png.load "png.png" [];; let g =
         Graphic_image.of_image img;; Graphics.draw_image g 0 0;; *)
    done
  done;
  !res

let rec overlay_piece_img = function
  | [] -> ()
  | x :: t -> (
      match x with
      | None -> ()
      | Some piece ->
          let pos = position piece in
          let x = fst pos in
          let y = snd pos in
          overlay_piece_img t)

(* [gen_oriented_board_lst board player] gives the [board] as a
   flattened list oriented with respect to [player]*)
let gen_oriented_board_lst board player =
  let flattize l =
    let rec helper = function
      | x :: t -> Array.to_list x :: helper t
      | [] -> []
    in
    Array.to_list l |> helper |> List.concat
  in
  let flattenedBoard = flattize board in
  if player = 2 then List.rev flattenedBoard else flattenedBoard

let draw st =
  let board = State.board st in
  print_endline (string_board board);
  let player = State.player_turn st in
  let boardlst = gen_oriented_board_lst board player in
  (* print_endline (string_of_int (List.length boardlst)); *)
  clear_graph ();
  set_color black;
  gen_grid 0 0;
  overlay_piece_img boardlst

let coordinate_pair status = (status.mouse_x / 100, status.mouse_y / 100)

let string_of_coordinate_pair tuple =
  string_of_int (fst tuple) ^ " " ^ string_of_int (snd tuple)

let listen f =
  let st = wait_next_event [ Button_down ] in
  if st.button then st |> coordinate_pair |> f
