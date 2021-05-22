type room = {
  room_id : string;
  state_fen : string;
}
[@@deriving yojson]

let rooms = (ref [] : room list ref)

let view_all_rooms () = Lwt.return !rooms

let create_room rm =
  rooms := rm :: !rooms;
  Lwt.return_unit
