open Piece
open State

(* [print_piece_type p] is the string of the type of piece of [p] *)
let print_piece_type (piece : Piece.t) : string =
  match piece_type piece with
  | Pawn -> "P"
  | Bishop -> "B"
  | Knight -> "N"
  | Rook -> "R"
  | Queen -> "Q"
  | King -> "K"

(* [print_piece_color p] is the string of the color of piece of [p] *)
let print_piece_color (piece : Piece.t) : string =
  if color piece = "black" then "B" else "W"

(* [print_piece_position p] is the string of the position of piece of
   [p] *)
let print_piece_position (piece : Piece.t) : string =
  let pos = position piece in
  "(" ^ string_of_int (fst pos) ^ "," ^ string_of_int (snd pos) ^ ")"

(* [print_piece p] is the string of piece [p] *)
let print_piece (piece : Piece.t) : string =
  print_piece_color piece ^ print_piece_type piece

(* [print_piece_option p] is the string of the Piece option*)
let print_piece_option (piece : Piece.t option) : string =
  match piece with Some p -> print_piece p | None -> "  "

(* [print_row lst] is the string representation of [lst]*)
let print_row (lst : Piece.t option list) : string =
  "|" ^ String.concat "|" (List.map print_piece_option lst) ^ "|"

(* [print_board s] is the string representation of the board in state
   [s]*)
let print_board (state : State.t) : string =
  let board = board state in
  "\n" ^ String.concat "\n" (List.map print_row board)
