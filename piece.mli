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

(* The two colors a chess piece can be *)
type color =
  | Black
  | White

(* The abstract type of values representing a chess piece. *)
type t

(* [make piece color icon positoin] is a piece with piece variant [piece], 
   color variant [color],
   icon path [icon] and positon on a 2d array [position]
   requires: [positon] to be (i, j) where i and j are elements of (0,8]) *)
val make : piece -> color -> string -> int * int -> t

(* [piece_type p] will return a variant of the piece type for piece [p]. *)
val piece_type : t -> piece

(* [color p] will return a string of the piece's color, white or black. *)
val color : t -> color

(* [icon p] will return the image file for the current piece. *)
val icon : t -> string

(* [position p] will return a tuple containing the current piece's
   position. *)
val position : t -> int * int

(* [locations p] will return a list of tuples containing the current
   piece's possible locations. *)
val locations : t -> (int * int) list

(* [move p loc] will return true if the current piece can move to the
   given location and false otherwise. *)
val move : t -> int * int -> bool
