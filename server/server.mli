(** [welcome ()] is the route GET / that returns a landing page that
    says "Welcome!" *)
val welcome : unit -> Opium.App.builder

(** [view_rooms ()] is the route GET /rooms that lists all the rooms in
    the databases.json file. *)
val view_rooms : unit -> Opium.App.builder

(** [get_room_by_id ()] is the route GET /room_id that returns the room
    with the room_id parameter from the databases.json file. *)
val get_by_room_id : unit -> Opium.App.builder

(** [post_room ()] is the route POST /rooms that adds a new room to the
    databases.json file. *)
val post_room : unit -> Opium.App.builder

(** [update_room ()] is the route PUT /rooms that updates an existing
    room with a new state_fen in the databases.json file. *)
val update_room : unit -> Opium.App.builder
