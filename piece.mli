(* Representation of a chess piece.

   A chess piece contains information about its location, the possible . *)

(* The abstract type of values representing a chess piece. *)
type t

(* The different types of chess pieces. *)
type piece

(* [piece_type p] will return a variant of the piece type. *)
val piece_type : t -> piece

(* [color p] will return a string of the piece's color, white or black. *)
val color : t -> string

(* [position p] will return a tuple containing the current piece's
   position. *)
val position : t -> int * int

(* [locations p] will return a list of tuples containing the current
   piece's possible locations. *)
val locations : t -> (int * int) list

(* [move p loc] will return true if the current piece can move to the
   given location and false otherwise. *)
val move : t -> int * int -> bool
