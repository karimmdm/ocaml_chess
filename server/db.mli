type room = {
  room_id : string;
  state_fen : string;
}
[@@deriving yojson]


(* [view_all_rooms ()] is Lwt.t promise containing all the rooms *)
val view_all_rooms : unit -> room list Lwt.t

(* [create_room ()] is the Lwt.t promise after a room has been created *)
val create_room : room -> unit Lwt.t  