open Opium

let ( let* ) = Lwt.bind

(**[welcome ()] is GET / *)
let welcome () =
  App.get "/" (fun _ -> Lwt.return (Response.of_plain_text "Welcome!"))

(**[get_all_rooms ()] is GET /rooms *)
let view_rooms () =
  App.get "/view_rooms" (fun _ ->
      print_endline "viewing all rooms";
      let* rooms = Db.get_all_rooms () in
      let json = [%to_yojson: Db.room list] rooms in
      Lwt.return (Response.of_json json))

(**[get_room_id () is GET /room_id] *)
let get_by_room_id () =
  App.get "/get/:room_id" (fun req ->
      print_endline "getting room by id";
      let room_id = Router.param req "room_id" in
      let* state_fen = Db.get_room room_id in
      Lwt.return (Response.of_plain_text ~status:`OK state_fen))

(**[post_room ()] is POST /rooms *)
let post_room () =
  App.post "/post/room" (fun request ->
      print_endline "creating room";
      let* input_json = Request.to_json_exn request in
      match Db.room_of_yojson input_json with
      | Ok room ->
          let* () = Db.create_room room in
          Lwt.return
            (Response.make
               ~body:(Body.of_string room.state_fen)
               ~status:`OK ())
      | Error err ->
          print_endline "there was an error making a room";
          raise (Invalid_argument err))

let update_room () =
  App.put "/put/:room_id" (fun req ->
      print_endline "updating room by id";
      let room_id = Router.param req "room_id" in
      let* state_fen = Request.to_json req in
      match state_fen with
      | Some (`String st) ->
          print_endline st;
          let* st' = Db.update_room room_id st in
          Lwt.return
            (Response.make ~body:(Body.of_string st') ~status:`OK ())
      | _ -> Lwt.return (Response.make ~status:`Not_found ()))

let () =
  App.empty |> welcome () |> view_rooms () |> get_by_room_id ()
  |> post_room () |> update_room () |> App.run_command
