open Yojson.Basic.Util

type piece =
  | Pawn
  | Bishop
  | Knight
  | Rook
  | Queen
  | King

type rule = {
  directions : (int * int) list;
  scalable : bool;
}

type t = {
  piece_type : piece;
  color : string;
  icon : string;
  position : int * int;
}

let rec convert_json_directions lst acc =
  match lst with
  | [] -> acc
  | h::t -> 
    let h = h |> to_list in
    convert_json_directions t ((List.hd h, List.hd (List.tl h))::acc)

let base_moves piece_type =
  let j = Yojson.Basic.from_file "base_moves.json" in
  let piece_str = Printer.print_piece_type piece_type in
  let piece_json = j |> to_assoc |> List.assoc piece_str in
  let directions = convert_json_directions (piece_json |> to_assoc |> List.assoc "directions" |> to_list) [] in
  let scalable = piece_json |> to_assoc |> List.assoc "scalable" |> to_string |> bool_of_string in
  { directions = directions; scalable = scalable }  

let piece_type p = p.piece_type

let color p = p.color

let icon p = p.icon

let position p = p.position

let update_position p new_pos = { p with position = new_pos }

let to_string p =
  let letter =
    match p.piece_type with
    | Pawn -> "P"
    | Bishop -> "B"
    | Knight -> "N"
    | Rook -> "R"
    | Queen -> "Q"
    | King -> "K"
  in
  if p.color = "black" then String.lowercase_ascii letter else letter

let make c pos =
  let letter_to_piece_type c =
    match c with
    | 'P' -> Pawn
    | 'B' -> Bishop
    | 'N' -> Knight
    | 'R' -> Rook
    | 'Q' -> Queen
    | _ -> King
  in
  let color = if Char.code c - 97 < 0 then "white" else "black" in
  let piece_type = letter_to_piece_type (Char.uppercase_ascii c) in
  let icon =
    "./images/" ^ Char.escaped color.[0] ^ Char.escaped c ^ ".png"
  in
  { piece_type; color; icon; position = pos }
