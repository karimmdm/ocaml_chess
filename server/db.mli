type room = {
  room_id : string;
  state_fen : string;
}
[@@deriving yojson]

(* [get_all_rooms ()] is Lwt.t promise containing all the rooms *)
val get_all_rooms : unit -> room list Lwt.t

(* [create_room ()] is the Lwt.t promise after a room has been created *)
val create_room : room -> unit Lwt.t

(* [get_room room_id] is the Lwt.t promise containing the state of the
   room given *)
val get_room : string -> string Lwt.t

(* [update_room room_id st] is the Lwt.t promise containing the state of
   the room just updated*)
val update_room : string -> string -> string Lwt.t
