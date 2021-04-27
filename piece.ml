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

let base_moves piece_type =
  match piece_type with
  | Pawn ->
      {
        directions = [ (-1, 0); (-2, 0); (-1, 1); (-1, -1) ];
        scalable = false;
      }
  | Bishop ->
      {
        directions = [ (-1, -1); (-1, 1); (1, -1); (1, 1) ];
        scalable = true;
      }
  | Knight ->
      {
        directions =
          [
            (-2, -1);
            (-2, 1);
            (2, -1);
            (2, 1);
            (-1, -2);
            (-1, 2);
            (1, -2);
            (1, 2);
          ];
        scalable = false;
      }
  | Rook ->
      {
        directions = [ (-1, 0); (1, 0); (0, 1); (0, -1) ];
        scalable = true;
      }
  | Queen ->
      {
        directions =
          [
            (-1, -1);
            (-1, 1);
            (1, -1);
            (1, 1);
            (1, 0);
            (-1, 0);
            (0, 1);
            (0, -1);
          ];
        scalable = true;
      }
  | King ->
      {
        directions =
          [
            (-1, -1);
            (-1, 0);
            (-1, 1);
            (0, -1);
            (0, 1);
            (1, -1);
            (1, 0);
            (1, 1);
          ];
        scalable = false;
      }

let make piece color icon position =
  { piece_type = piece; color; icon; position }

let piece_type p = p.piece_type

let color p = p.color

let icon p = p.icon

let position p = p.position

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

let from_letter c pos =
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
  let icon_str =
    "./images/" ^ Char.escaped color.[0] ^ Char.escaped c ^ ".png"
  in
  { piece_type; color; icon_str; pos }
