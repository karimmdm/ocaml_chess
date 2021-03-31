type piece =
  | Pawn
  | Bishop
  | Knight
  | Rook
  | Queen
  | King

type t = {
  piece_type : piece;
  color : color;
  icon : string;
  position : int * int;
}

let make piece color icon position =
  { piece_type = piece; color; icon; position }

let piece_type p = p.piece_type

let color p = p.color

let icon p = p.icon

let position p = p.position
