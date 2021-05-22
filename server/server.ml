open Opium

let ( let* ) = Lwt.bind

(* [welcome ()] is GET / *)
let welcome () =
  App.get "/" (fun _request ->
      Lwt.return (Response.of_plain_text "Welcome!"))

(* [get_all_rooms ()] is GET /rooms *)
let get_all_rooms () =
  App.get "/rooms" (fun _ ->
      let* rooms = Db.view_all_rooms () in
      let json = [%to_yojson: Db.room list] rooms in
      Lwt.return (Response.of_json json))

(* [post_room ()] is POST /rooms *)
let post_room () =
  App.post "/rooms" (fun request ->
      let* input_json = Request.to_json_exn request in
      let input_room =
        match Db.room_of_yojson input_json with
        | Ok room -> room
        | Error err -> raise (Invalid_argument err)
      in
      let* () = Db.create_room input_room in
      Lwt.return (Response.make ~status:`OK ()))

let () =
  App.empty |> welcome () |> get_all_rooms () |> post_room ()
  |> App.run_command
