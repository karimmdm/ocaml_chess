(**mode is the mode of the game *)
type mode =
  | Create
  | Join
  | Local

(**[play_game st mode room_id] starts a game with the initial game state
   [st] with the correct [mode] and [room_id]*)
val play_game : State.t -> mode -> string option -> unit
