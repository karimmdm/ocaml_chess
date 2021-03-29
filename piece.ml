type piece =
  | Pawn
  | Bishop
  | Knight
  | Rook
  | Queen
  | King

type t = {
  piece_type : piece;
  color : string;
  icon : string;
  position : int * int;
}

let piece_type p = p.piece_type

let color p = p.color

let icon p = p.icon

let position p = p.position

let locations p =
  let clr = p.color in
  match p.piece_type with
  | Pawn -> 
      if String.equal clr "white" then
      else
