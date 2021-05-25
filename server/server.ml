open Opium

let ( let* ) = Lwt.bind

(* [welcome ()] is GET / *)
let welcome () =
  App.get "/" (fun _ ->
      Lwt.return (Response.of_plain_text "Welcome!"))

(* [get_all_rooms ()] is GET /rooms *)
let get_all_rooms () =
  App.get "/rooms" (fun _ ->
      let* rooms = Db.get_all_rooms () in
      let json = [%to_yojson: Db.room list] rooms in
      Lwt.return (Response.of_json json))

(* [get_room_id () is GET /room_id] *)
let get_by_room_id () =
  App.get ("/:room_id") (fun req -> 
    let room_id = Router.param req "room_id" in 
    let* state_fen = Db.get_room room_id in 
    print_endline state_fen;
    Lwt.return (Response.of_plain_text ~status: `OK state_fen)
  )

(* [post_room ()] is POST /rooms *)
let post_room () =
  App.post "/rooms" (fun request ->
      let* input_json = Request.to_json_exn request in
      match Db.room_of_yojson input_json with
      | Ok room -> 
        let* () = Db.create_room room in
        Lwt.return (Response.make ~body:Opium.Body.of_string (room.state_fen) ~status:`OK ()))
      | Error err -> raise (Invalid_argument err))
      

let update_room () = failwith ""

let () =
  App.empty |> welcome () |> get_all_rooms () |> get_by_room_id () |> post_room ()
  |> App.run_command
