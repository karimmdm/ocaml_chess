open Piece

type t = {
  board : Piece.t option list list;
  player_turn : int;
  check : bool;
  checkmate : bool;
  stalemate : bool;
}

let init_state () = {
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

let board st = st.board

let player_turn st = st.player_turn

let check st = st.check

let checkmate st = st.checkmate

let stalemate st = st.stalemate

(* [check_bounds grid loc] returns true if the given location is within the
  bounds of the given grid and false otherwise. *)
let check_bounds (grid : 'a option list list) (loc : int * int) : bool =
  let width = List.length grid in
  let height = List.length (List.hd grid) in
  let x = fst loc in
  let y = snd loc in
  if x >= width || x < 0 || y >= height || y < 0 then false else true

(* [get_elt grid x y] returns an option of element in the grid at (x, y) if that
  element exists and if x and y are within the bounds of the grid. *)
let get_elt (grid : 'a list list) (x : int) (y : int) : 'a =
  if check_bounds grid (x, y) then List.nth (List.nth grid x) y
  else None

(* [valid_positions_ lst acc] returns [acc] which contains all the valid
  board positions in [lst]. If an enemy piece (piece that is not [clr]),
  this function assumes that the piece can capture it, so this does not apply
  to pawns, which cannot capture pieces in its way vertically. *)
let rec valid_positions board clr x y lst acc =
  match lst with
  | h::t -> 
    if check_bounds board h then 
      let elt = get_elt board x y in
      match elt with
      | Some p -> 
        if not (String.equal (Piece.color p) clr) then 
          valid_positions board clr x y t (h::acc)
        else valid_positions board clr x y t acc
      | None -> valid_positions board clr x y t (h::acc)
    else valid_positions board clr x y t acc
  | [] -> acc

(* [check_diag grid clr x y acc diag] returns a list of positions along
  the diagonal specified by [diag] that are empty or a valid capture. *)
let rec check_diag grid clr x y acc diag =
  if check_bounds grid (x, y) then 
    let elt = get_elt grid x y in
      match elt with
      | Some p -> 
          if not (String.equal (Piece.color p) clr) then (x, y)::acc
          else acc
      | None -> 
        if String.equal clr "white" then
          if String.equal diag "main" then 
            check_diag grid clr (x + 1) (y + 1) ((x, y)::acc) diag @
            check_diag grid clr (x - 1) (y - 1) ((x, y)::acc) diag
          else
            check_diag grid clr (x - 1) (y + 1) ((x, y)::acc) diag @
            check_diag grid clr (x + 1) (y - 1) ((x, y)::acc) diag
        else 
          if String.equal diag "main" then 
            check_diag grid clr (x + 1) (y - 1) ((x, y)::acc) diag @
            check_diag grid clr (x - 1) (y + 1) ((x, y)::acc) diag
          else 
            check_diag grid clr (x - 1) (y - 1) ((x, y)::acc) diag @
            check_diag grid clr (x + 1) (y + 1) ((x, y)::acc) diag
  else acc

(* [check_rank board clr x y acc dir] returns [acc], a list of valid positions
  in the row or column, specified by [dir] starting at (x, y). *)
let rec check_rank board clr x y acc dir = 
  if check_bounds board (x, y) then
    let elt = get_elt board x y in
      match elt with
      | Some p ->
        if not (String.equal (Piece.color p) clr) then ((x, y)::acc)
        else acc
      | None -> 
        match dir with
        | "ver" -> 
            check_rank board clr x (y + 1) acc dir @ 
            check_rank board clr x (y - 1) acc dir
        | "hor" ->
            check_rank board clr (x + 1) y acc dir @ 
            check_rank board clr (x - 1) y acc dir
        | _ -> []
  else acc

let locations st p =
  let x = fst (Piece.position p) in
  let y = snd (Piece.position p) in
  let board = st.board in
  let clr = Piece.color p in
  let piece = Piece.piece_type p in
  match piece with
  | Pawn ->
    (* [pawn_capture board clr x y] returns a list of positions that the given 
      pawn piece can move to after a legal capture. *)
    let pawn_capture board clr x y =
      let rec helper lst acc = 
        match lst with
        | h::t -> helper t (if check_bounds board h then h::acc else acc)
        | [] -> acc in
      if String.equal clr "white" then
        let check_locs = (x + 1, y + 1)::(x - 1, y + 1)::[] in 
        helper check_locs []
      else
        let check_locs = (x + 1, y - 1)::(x - 1, y - 1)::[] in
        helper check_locs [] in
    (* [pawn_move board clr x y] returns a list of positions that the given 
      pawn piece can move to, excluding captures. *)
    let pawn_move board clr x y =
      if String.equal clr "white" then
        if y == 1 then [(x, y + 1); (x, y + 2)]
        else if check_bounds board (x, y + 1) then [(x, y + 1)] else []
      else [] in
    (pawn_capture board clr x y) @ (pawn_move board clr x y)
  | Bishop ->
    (* [bishop_move board clr x y] returns a list of positions that the given
      bishop piece can move to, including captures. *)
    let bishop_move board clr x y =
      check_diag board clr x y [] "main" @ check_diag board clr x y [] "snd" in
    bishop_move board clr x y
  | Knight ->
    (* [knight_move board clr x y] returns a list of positions that the given
      knight piece can move to, including captures. *)
    let knight_move board clr x y = 
      let check_locs = 
        [(x + 1, y + 2); (x - 1, y + 2); (x + 2, y + 1); (x - 2, y + 1);
         (x + 1, y - 2); (x - 1, y - 2); (x + 2, y - 1); (x - 2, y - 1)] in
      valid_positions board clr x y check_locs [] in 
    knight_move board clr x y
  | Rook ->
    (* [rook_move board clr x y] returns a list of positions that the given
      rook piece can move to, including captures. *)
    let rook_move board clr x y = 
      check_rank board clr x y [] "ver" @ check_rank board clr x y [] "hor" in
    rook_move board clr x y
  | Queen -> 
    (* [queen_move board clr x y] returns a list of positions that the given
      queen piece can move to, including captures. *)
    let queen_move board clr x y = 
      check_rank board clr x y [] "ver" @ check_rank board clr x y [] "hor" @ 
      check_diag board clr x y [] "main" @ check_diag board clr x y [] "snd" in
    queen_move board clr x y
  | King ->
    let king_move board clr x y =
      let check_locs = 
        [(x + 1, y + 2); (x - 1, y + 2); (x + 2, y + 1); (x - 2, y + 1);
         (x + 1, y - 2); (x - 1, y - 2); (x + 2, y - 1); (x - 2, y - 1)] in
      valid_positions board clr x y check_locs [] in 
    king_move board clr x y

let valid_move st p loc =
  List.mem loc (locations st p)
