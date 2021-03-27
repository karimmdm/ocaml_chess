(* Representation of a board.

   A board will contain information about the game state and a mutable
   array of pieces for each player. A chess board is always an 8x8 grid. *)

(* The abstract type of values representing boards. *)
type t

(* Mutable array for player 1's pieces. *)
type pieces1

(* Mutable array for player 2's pieces. *)
type pieces2
