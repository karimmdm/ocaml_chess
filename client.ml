open Lwt

let join_game () = ANSITerminal.print_string [ ANSITerminal.red ]
"\n\nWelcome to the OCaml Multiplayer Chess Game!\n";
print_endline
"Please enter a valid room ID to join the game as Black.\n";
print_string "> ";
match read_line () with
| exception End_of_file -> ()
| room_id -> 
    let body = Cohttp_lwt_unix.Client.get (Uri.of_string ("localhost:3000/" ^ room_id)) >>= fun (resp, body) ->
      (* let code = resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
      Printf.printf "Response code: %d\n" code;
      Printf.printf "Headers: %s\n" (resp |> Cohttp.Response.headers |> Cohttp.Header.to_string); *)
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      Printf.printf "Body of length: %d\n" (String.length body);
      body in
    let run_body = Lwt_main.run body in print_endline ("Received body\n" ^ run_body)