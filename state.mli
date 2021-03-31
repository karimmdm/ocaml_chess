(* Representation of a dynamic chess game state.

   A game state will contain information about the player turn, whether
   or not a player is in check, or if the game has reached a win
   condition (checkmate or stalemate). If one of checkmate or stalemate
   is true, the other must be false. *)

(* The abstract type of values representing a game state. *)
type t

(* [init_state ()] initializes a new state with an empty 8x8 board and
   sets the player turn to 1 and all the booleans to false. *)
val init_state : unit -> t

(* [board st] is the current board configuration of the game state [st] *)
val board : t -> Piece.t option list list

(* [player_turn st] will return an int representing whose turn it is, 1
   for player 1 (white) and 2 for player 2 (black). *)
val player_turn : t -> int

(* [check st] will return true if the current player is in check and
   false otherwise. *)
val check : t -> bool

(* [checkmate st] will return true if the current player is in checkmate
   and false otherwise. *)
val checkmate : t -> bool

(* [stalemate st] will return true if the current player is in stalemate
   and false otherwise. *)
val stalemate : t -> bool

(* [locations st p] returns a list of positions represented by int
   tuples that the given piece can move to via official chess rules. *)
val locations : t -> Piece.t -> (int * int) list

(* [valid_move st p loc] returns true if the given piece can move to the
   given location if the given piece can legally move to that location,
   if that location is not occupied, and and if the given location is
   within the bounds of the board. *)
val valid_move : t -> Piece.t -> int * int -> bool
