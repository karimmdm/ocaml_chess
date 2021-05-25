(* mode is the mode of the game *)
type mode =
  | Create
  | Join
  | Local

(* [play_game st] starts a game with the initial game state [st]*)
val play_game : State.t -> mode -> unit
