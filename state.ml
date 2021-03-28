type t = {
  board : Piece.t list list;
  player_turn : int;
  check : bool;
  checkmate : bool;
  stalemate : bool;
}

let init_state f = failwith ""

let player_turn st = st.player_turn

let check st = st.check

let checkmate st = st.checkmate

let stalemate st = st.stalemate

let valid_move st p loc = failwith ""