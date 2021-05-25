(* [get_state_by_id_request room_id] retrieves the state of a game in a
   particulare room [room_id]*)
val get_state_by_id_request : string -> string

(* [join_game ()] lets a user join a game by entering a room name in the
   terminal*)
val join_game : unit -> unit

(* [create_game ()] creates a game after the user inputs a room name*)
val create_game : unit -> unit

(* [update_room_state room_id state_fen ] upates the state of the room
   with name [room_id] with state [state_fen]*)
val update_room_state : string -> string -> string
