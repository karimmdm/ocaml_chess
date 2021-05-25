(**[get_state_by_id_request room_id] retrieves the state of a game in a
   particulare room [room_id]*)
val get_state_by_id_request : string -> string

(**[create_room_request room_id ] creates a room with name [room_id] *)
val create_room_request : string -> string

(**[update_room_state room_id state_fen ] upates the state of the room
   with name [room_id] with state [state_fen]*)
val update_room_state : string -> string -> string
