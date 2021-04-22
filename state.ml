open Piece

type t = {
  board : Piece.t option list list;
  player_turn : int;
  check : bool;
  checkmate : bool;
  stalemate : bool;
}

let letter_to_piece_type c : piece =
  match c with
  | 'P' -> Pawn
  | 'B' -> Bishop
  | 'N' -> Knight
  | 'R' -> Rook
  | 'Q' -> Queen
  | _ -> King

let letter_to_piece c pos : Piece.t =
  let color = if Char.code c - 97 < 0 then "white" else "black" in
  let piece_type = letter_to_piece_type (Char.uppercase_ascii c) in
  let prefix = if color = "white" then "w" else "b" in
  let icon_str = "./images/" ^ prefix ^ Char.escaped c ^ ".png" in
  make piece_type color icon_str pos

let rec int_to_nones i : Piece.t option list =
  if i = 0 then [] else None :: int_to_nones (i - 1)

let rec string_to_lst (s : string) (i : int) (j : int) :
    Piece.t option list =
  let length = String.length s in
  if length = 0 then []
  else
    let c = s.[0] in
    let rest = String.sub s 1 (length - 1) in
    if Char.code c < 58 then
      let n = int_of_string (Char.escaped c) in
      int_to_nones n @ string_to_lst rest i (j + n)
    else Some (letter_to_piece c (i, j)) :: string_to_lst rest i (j + 1)

let make_board (fen : string) =
  let row_lst = String.split_on_char '/' fen in
  let rec board_helper lst i =
    match lst with
    | [] -> []
    | h :: t -> string_to_lst h i 0 :: board_helper t (i + 1)
  in
  board_helper row_lst 0

let state_from_fen (fen : string) =
  {
    board = make_board fen;
    player_turn = 1;
    check = false;
    checkmate = false;
    stalemate = false;
  }

let init_state () =
  state_from_fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

let board st = st.board

let player_turn st = st.player_turn

let check st = st.check

let checkmate st = st.checkmate

let stalemate st = st.stalemate

(* [check_bounds grid loc] returns true if the given location is within
   the bounds of the given grid and false otherwise. *)
let check_bounds (grid : 'a option list list) (loc : int * int) : bool =
  let width = List.length grid in
  let height = List.length (List.hd grid) in
  let x = fst loc in
  let y = snd loc in
  if x >= width || x < 0 || y >= height || y < 0 then false else true

(* [get_elt grid x y] returns an option of element in the grid at (x, y)
   if that element exists and if x and y are within the bounds of the
   grid. *)
let get_elt (grid : 'a list list) (x : int) (y : int) : 'a =
  if check_bounds grid (x, y) then List.nth (List.nth grid x) y
  else None

(* [valid_positions_ lst acc] returns [acc] which contains all the valid
   board positions in [lst]. If an enemy piece (piece that is not
   [clr]), this function assumes that the piece can capture it, so this
   does not apply to pawns, which cannot capture pieces in its way
   vertically. *)
let rec valid_positions board clr x y lst acc =
  match lst with
  | h :: t ->
      if check_bounds board h then
        let elt = get_elt board x y in
        match elt with
        | Some p ->
            if not (String.equal (Piece.color p) clr) then
              valid_positions board clr x y t (h :: acc)
            else valid_positions board clr x y t acc
        | None -> valid_positions board clr x y t (h :: acc)
      else valid_positions board clr x y t acc
  | [] -> acc

let locations st p =
  let x = fst (Piece.position p) in
  let y = snd (Piece.position p) in
  let board = st.board in
  let clr = Piece.color p in
  let piece = Piece.piece_type p in
  match piece with
  | Pawn ->
  | Bishop ->
  | Knight ->
  | Rook ->
  | Queen ->
  | King ->

let valid_move st p loc = List.mem loc (locations st p)
