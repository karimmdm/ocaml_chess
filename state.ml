open Piece

type t = {
  board : Piece.t option array array;
  player_turn : int;
  check : bool;
  checkmate : bool;
  stalemate : bool;
}

(* [init_board] is the starting board facing player 1 (white king) *)
let rec init_board () =
  let grid = Array.make 8 (Array.make 8 None) in
  for i = 0 to 7 do
    let wp = Piece.make Pawn White "/images/wp.png" (6, i) in
    grid.(6).(i) <- Some wp;
    let bp = Piece.make Pawn Black "/images/bp.png" (1, i) in
    grid.(1).(i) <- Some bp
  done;
  for j = 2 to 5 do
    for i = 0 to 7 do
      grid.(j).(i) <- None
    done
  done;
  let wr1 = Piece.make Rook White "/images/wr.png" (7, 0) in
  let wr2 = Piece.make Rook White "/images/wr.png" (7, 7) in
  let wkn1 = Piece.make Knight White "/images/wkn.png" (7, 1) in
  let wkn2 = Piece.make Knight White "/images/wkn.png" (7, 6) in
  let wb1 = Piece.make Bishop White "images/wb.png" (7, 2) in
  let wb2 = Piece.make Bishop White "images/wb.png" (7, 5) in
  let wq = Piece.make Queen White "images/wq.png" (7, 3) in
  let wk = Piece.make King White "images/wK.png" (7, 4) in
  let br1 = Piece.make Rook Black "/images/br.png" (0, 0) in
  let br2 = Piece.make Rook Black "/images/br.png" (0, 7) in
  let bkn1 = Piece.make Knight Black "/images/bkn.png" (0, 1) in
  let bkn2 = Piece.make Knight Black "/images/bkn.png" (0, 6) in
  let bb1 = Piece.make Bishop Black "images/bb.png" (0, 2) in
  let bb2 = Piece.make Bishop Black "images/bb.png" (0, 5) in
  let bq = Piece.make Queen Black "images/bq.png" (0, 3) in
  let bk = Piece.make King Black "images/bK.png" (0, 4) in
  grid.(7).(0) <- Some wr1;
  grid.(7).(7) <- Some wr2;
  grid.(7).(1) <- Some wkn1;
  grid.(7).(6) <- Some wkn2;
  grid.(7).(2) <- Some wb1;
  grid.(7).(5) <- Some wb2;
  grid.(7).(3) <- Some wq;
  grid.(7).(4) <- Some wk;
  grid.(0).(0) <- Some br1;
  grid.(0).(7) <- Some br2;
  grid.(0).(1) <- Some bkn1;
  grid.(0).(6) <- Some bkn2;
  grid.(0).(2) <- Some bb1;
  grid.(0).(5) <- Some bb2;
  grid.(0).(3) <- Some bq;
  grid.(0).(4) <- Some bk;
  grid

let init_state () =
  {
    board = init_board ();
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

let valid_move st p loc =
  (* [check_bounds grid loc] *)
  let check_bounds (grid : Piece.t option array array) (loc : int * int)
      : bool =
    failwith ""
  in
  if check_bounds st.board loc then true else false
