type t = {
  board : Piece.t option list list;
  player_turn : int;
  check : bool;
  checkmate : bool;
  stalemate : bool;
}

(* type piece =
  | Pawn
  | Bishop
  | Knight
  | Rook
  | Queen
  | King *)

let init_state f = {
  board = [
    [None; None; None; None; None; None; None; None];
    [None; None; None; None; None; None; None; None];
    [None; None; None; None; None; None; None; None];
    [None; None; None; None; None; None; None; None];
    [None; None; None; None; None; None; None; None];
    [None; None; None; None; None; None; None; None];
    [None; None; None; None; None; None; None; None];
    [None; None; None; None; None; None; None; None]
  ];
  player_turn = 1;
  check = false;
  checkmate = false;
  stalemate = false;
}

let player_turn st = st.player_turn

let check st = st.check

let checkmate st = st.checkmate

let stalemate st = st.stalemate

let locations st p =
  let lst = [] in
  let board = st.board in
  let piece = Piece.piece_type p in
  match piece with
  | piece Pawn -> []
  | _ -> [] in
  lst

let valid_move st p loc = 
  (* [check_bounds grid loc] returns true if the given location is within the
    bounds of the given grid and false otherwise. *)
  let check_bounds (grid : Piece.t option list list) (loc : int * int) : bool =
    let width = List.length grid in
    let height = List.length (List.hd grid) in
    let x = (fun (fst, _) -> fst) loc in
    let y = (fun (_, snd) -> snd) loc in
    if x >= width || x < 0 || y >= height || y < 0 then false else true in

  (* [is_occupied grid loc] returns true if the given location in the grid 
    contains a piece and false otherwise. *)
  let is_occupied (grid : Piece.t option list list) (loc : int * int) : bool =
    let x = fst loc in
    let y = snd loc in
    let piece = List.nth (List.nth grid x) y in
    match piece with
    | Some piece -> false
    | None -> true in

  (* [can_piece_move grid loc p] returns true if the given piece can move to
    the given location in the given grid and false otherwise. *)
  let can_move_piece (grid : Piece.t option list list) (loc : int * int)
    (p : Piece.t) = List.mem p. loc
  if check_bounds st.board loc && not (is_occupied st.board loc) && 
    can_move_piece st.board loc p then true
  else false
