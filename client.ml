let get_state_by_id_request room_id =
  let url = "localhost:3000/get/" ^ room_id in
  match Curly.(run (Request.make ~url ~meth:`GET ())) with
  | Ok x -> x.Curly.Response.body
  | Error e -> failwith ("error finding a room name: " ^ room_id)

let create_room_request room_id =
  let url = "localhost:3000/post/room" in
  let body =
    {|{"room_id": |} ^ "\"" ^ room_id ^ "\"" ^ {|, "state_fen": |}
    ^ "\"" ^ State.init_fen () ^ "\"}"
  in
  match Curly.(run (Request.make ~body ~url ~meth:`POST ())) with
  | Ok x -> x.Curly.Response.body
  | Error e -> failwith "error making a room"

let update_room_state room_id state_fen =
  let url = "localhost:3000/put/" ^ room_id in
  let body = state_fen in
  match Curly.(run (Request.make ~body ~url ~meth:`PUT ())) with
  | Ok x -> x.Curly.Response.body
  | Error e -> failwith "error updating room"
