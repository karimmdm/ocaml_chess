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
        directions = [ (-1, 0); (0, 1); (1, 0); (-1, 0) ];
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
