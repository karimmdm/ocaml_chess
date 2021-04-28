(* Representation of a chess piece.

   A chess piece contains information about its piece type, color, image
   file, location, and the possible locations it can move to. A location
   is represented as a tuple with a char and int, such as (a, 8). *)

(* The different types of chess pieces. *)
type piece =
  | Pawn
  | Bishop
  | Knight
  | Rook
  | Queen
  | King

(* The rule type specifying the general direction a piece can move and
   if that piece can move multiple squares in that direction. *)
type rule = {
  directions : (int * int) list;
  scalable : bool;
}

(* [base_moves p] is the basic directinal rule that the piece [p] abides
   by*)
val base_moves : piece -> rule

(* The abstract type of values representing a chess piece. *)
type t

(* [make c position] is a piece with piece variant based on char [c] and
   positon on a 2d array [position] requires: [positon] to be (i, j)
   where i and j are elements of (0,8]) *)
val make : char -> int * int -> t

(* [piece_type p] will return a variant of the piece type for piece [p]. *)
val piece_type : t -> piece

(* [color p] will return a string of the piece's color, white or black. *)
val color : t -> string

(* [icon p] will return the image file for the current piece. *)
val icon : t -> string

(* [position p] will return a tuple containing the current piece's
   position. *)
val position : t -> int * int

(* [to_string p] will return a string representation of a piece. Black
   pieces are represented in lowercase while white pieces are in
   uppercase*)
val to_string : t -> string
