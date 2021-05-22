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

let piece_type_to_string piece_type =
  match piece_type with
  | Pawn -> "Pawn"
  | Bishop -> "Bishop"
  | Knight -> "Knight"
  | Rook -> "Rook"
  | Queen -> "Queen"
  | King -> "King"

(* [extract_directions lst] returns the the json list [lst] into a
   [rule] compatible directions list *)
let extract_directions lst =
  List.map
    (fun pair_lst ->
      match pair_lst with
      | `List [ `Int a; `Int b ] -> (a, b)
      | _ -> failwith "Invalid base_moves.json (directions)")
    lst

let base_moves piece_type =
  try
    let json = Yojson.Basic.from_file "base_moves.json" in
    let piece_json = json |> member (piece_type_to_string piece_type) in
    let scalable_bool =
      piece_json |> member "scalable" |> function
      | `Bool b -> b
      | _ -> failwith "Invalid base_move.json (scalable)"
    in
    let directions_json = piece_json |> member "directions" in
    let dir_lst =
      match directions_json with
      | `List a -> extract_directions a
      | _ -> failwith "Invalid base_moves.json (directions)"
    in
    { directions = dir_lst; scalable = scalable_bool }
  with err -> raise err

let piece_type p = p.piece_type

let color p = p.color

let icon p = p.icon

let position p = p.position

let update_position p new_pos = { p with position = new_pos }

let to_letter p =
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
