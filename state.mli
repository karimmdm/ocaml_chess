(* Representation of a dynamic chess game state.

   A game state will contain information about the player turn, whether
   or not a player is in check, or if the game has reached a win
   condition (checkmate or stalemate). If one of checkmate or stalemate
   is true, the other must be false. *)

(* The abstract type of values representing a game state. *)
type t

(* [player_turn st] will return an int representing whose turn it is, 1
   for player 1 (white) and 2 for player 2 (black). *)
val player_turn : t -> int

(* [check st] will return true if the current player is in check and
   false otherwise. *)
val check : t -> bool

(* [game_win st] will return true if the game has reached an end state
   and false otherwise. *)
val game_win : t -> bool

(* [checkmate st] will return true if the current player is in checkmate
   and false otherwise. *)
val checkmate : t -> bool

(* [stalemate st] will return true if the current player is in stalemate
   and false otherwise. *)
val stalemate : t -> bool
