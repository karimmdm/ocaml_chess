let ( let* ) = Lwt.bind

type room = {
  room_id : string;
  state_fen : string;
}
[@@deriving yojson]

let db_file = "database.json"

(* [read_json ()] opens and reads the database *)
let view_all_rooms () =
  Lwt_io.with_file ~mode:Input db_file (fun input_channel ->
      let* db_string =
        Lwt_io.read_lines input_channel |> Lwt_stream.to_list
      in
      let db_json =
        Yojson.Safe.from_string (String.concat "\n" db_string)
      in
      match [%of_yojson: room list] db_json with
      | Ok rooms -> Lwt.return rooms
      | Error err -> raise (Invalid_argument err))

(* [create_room ()] creates a room [rm] and writes it to the database *)
let create_room rm =
  let* rooms = view_all_rooms () in
  let rooms = rm :: rooms in
  Lwt_io.with_file ~mode:Output db_file (fun output_channel ->
      let rooms_string =
        rooms |> [%to_yojson: room list] |> Yojson.Safe.pretty_to_string
      in
      Lwt_io.write output_channel rooms_string)
