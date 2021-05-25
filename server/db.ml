open Yojson.Basic.Util

let ( let* ) = Lwt.bind

type room = {
  room_id : string;
  state_fen : string;
}
[@@deriving yojson]

let db_file = "database.json"

let get_all_rooms () =
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

let create_room rm =
  let* rooms = get_all_rooms () in
  let rooms = rm :: rooms in
  Lwt_io.with_file ~mode:Output db_file (fun output_channel ->
      let rooms_string =
        rooms |> [%to_yojson: room list] |> Yojson.Safe.pretty_to_string
      in
      Lwt_io.write output_channel rooms_string)

let rec parse_helper room_id = function
  | h :: t ->
      let assoc_lst = h |> to_assoc in
      if List.assoc "room_id" assoc_lst |> to_string = room_id then
        List.assoc "state_fen" assoc_lst |> to_string
      else parse_helper room_id t
  | [] -> failwith (room_id ^ " is an invalid room id")

let get_room room_id =
  Lwt_io.with_file ~mode:Input db_file (fun input_channel ->
      let* db_string =
        Lwt_io.read_lines input_channel |> Lwt_stream.to_list
      in
      let db_json =
        Yojson.Safe.from_string (String.concat "\n" db_string)
      in
      let json_lst = db_json |> Yojson.Safe.to_basic |> to_list in
      Lwt.return (parse_helper room_id json_lst))

let update_json room_id state_fen lst =
  List.map
    (fun h -> if h.room_id = room_id then { h with state_fen } else h)
    lst

let update_room room_id state_fen =
  let* rooms = get_all_rooms () in
  let rooms = update_json room_id state_fen rooms in
  ignore
    (Lwt_io.with_file ~mode:Output db_file (fun output_channel ->
         let rooms_string =
           rooms |> [%to_yojson: room list]
           |> Yojson.Safe.pretty_to_string
         in
         Lwt_io.write output_channel rooms_string));
  Lwt.return state_fen
