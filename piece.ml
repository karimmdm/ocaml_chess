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
  (* [check_bounds grid loc] returns true if the given location is within the
    bounds of the given grid and false otherwise. *)
  let check_bounds (grid : t option list list) (loc : int * int) : bool =
    let width = List.length grid in
    let height = List.length (List.hd grid) in
    let x = (fun (fst, _) -> fst) loc in
    let y = (fun (_, snd) -> snd) loc in
    if x >= width || x < 0 || y >= height || y < 0 then false else true in
  if p.piece_type == Pawn then 
    let lst = [] in
    if p.position
    lst
  else if p.piece_type == Bishop then
  else if p.piece_type == Knight then
  else if if p.piece_type == Rook then
  else if p.piece_type == Queen then
  else if p.piece_type == King then

(* let move p loc = List.mem loc (locations p) *)
