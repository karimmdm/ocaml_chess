open Piece

type t = {
  board : Piece.t option list list;
  player_turn : int;
  check : bool;
  checkmate : bool;
  stalemate : bool;
  castle_kingside : bool * bool;
  castle_queenside : bool * bool;
  piece_clicked : Piece.t option;
}

(* [string_to_list s i j] produces a list of piece options based on the
   string [s] with the starting position at [i][j]*)
let rec string_to_lst (s : string) (i : int) (j : int) :
    Piece.t option list =
  let rec int_to_nones i : Piece.t option list =
    if i = 0 then [] else None :: int_to_nones (i - 1)
  in
  let length = String.length s in
  if length = 0 then []
  else
    let c = s.[0] in
    let rest = String.sub s 1 (length - 1) in
    if Char.code c < 58 then
      let n = int_of_string (Char.escaped c) in
      int_to_nones n @ string_to_lst rest i (j + n)
    else Some (Piece.make c (i, j)) :: string_to_lst rest i (j + 1)

let rec lst_to_string (lst : Piece.t option list) (none_count : int) :
    string =
  match lst with
  | [] -> if none_count = 0 then "" else string_of_int none_count
  | Some p :: t ->
      if none_count = 0 then Piece.to_letter p ^ lst_to_string t 0
      else
        string_of_int none_count ^ Piece.to_letter p ^ lst_to_string t 0
  | None :: t -> lst_to_string t (none_count + 1)

let fen_to_board (str : string) =
  let row_lst = String.split_on_char '/' str in
  let rec board_helper lst i =
    match lst with
    | [] -> []
    | h :: t -> string_to_lst h i 0 :: board_helper t (i + 1)
  in
  board_helper row_lst 0

let castle_to_pair s =
  match String.split_on_char ';' s with
  | [ c1; c2 ] -> (bool_of_string c1, bool_of_string c2)
  | _ -> failwith "never reach here"

let state_from_fen fen st_option =
  let fen_split_lst = String.split_on_char ':' fen in
  let board_str = List.hd fen_split_lst in
  let new_board = fen_to_board board_str in
  match st_option with
  | None -> (
      let flag_status_str = List.hd (List.tl fen_split_lst) in
      match String.split_on_char ',' flag_status_str with
      | [] -> failwith "flag statuses needed"
      | [ pt; c; cm; sm; ck; cq ] ->
          {
            board = new_board;
            player_turn = int_of_string pt;
            check = bool_of_string c;
            checkmate = bool_of_string cm;
            stalemate = bool_of_string sm;
            castle_kingside = castle_to_pair ck;
            castle_queenside = castle_to_pair cq;
            piece_clicked = None;
          }
      | _ -> failwith "flag statuses were not appropriately entered")
  | Some st -> { st with board = new_board }

let rec board_to_fen board =
  match board with
  | [] -> ""
  | [ h ] -> lst_to_string h 0
  | h :: t -> lst_to_string h 0 ^ "/" ^ board_to_fen t

let to_fen t =
  board_to_fen t.board ^ ":"
  ^ string_of_int t.player_turn
  ^ "," ^ string_of_bool t.check ^ ","
  ^ string_of_bool t.checkmate
  ^ ","
  ^ string_of_bool t.stalemate
  ^ ","
  ^ string_of_bool (fst t.castle_kingside)
  ^ ";"
  ^ string_of_bool (snd t.castle_kingside)
  ^ ","
  ^ string_of_bool (fst t.castle_queenside)
  ^ ";"
  ^ string_of_bool (snd t.castle_queenside)

let init_state () =
  state_from_fen
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR:1,false,false,false,true;true,true;true"
    None

let board st = st.board

(* [gen_board st pos] returns a new board that reflects
   [st.piece_clicked] moving to location [pos] on the board. requires:
   [st.piece_clicked] to be Some piece [pos] to be the position of a
   valid move location for [st.piece_clicked]*)
let gen_board st p move_to_pos =
  let move_row = fst move_to_pos in
  let move_col = snd move_to_pos in
  let piece_clicked_pos = Piece.position p in
  let piece_clicked_row = fst piece_clicked_pos in
  let row_traversal i row_lst =
    (* [column_traversal_a elt] sets the [st.piece_clicked] piece to
       None type *)
    let column_traversal_a = function
      | None -> None
      | Some piece ->
          if Piece.position piece = piece_clicked_pos then None
          else Some piece
    in
    (* [column_traversal_b j elt] sets the element [elem_option] to
       [st.piece_clicked]*)
    let column_traversal_b j elt =
      if j = move_col then
        Some (Piece.update_position p (move_row, move_col))
      else elt
    in
    if i = piece_clicked_row then
      let lst1 = List.map column_traversal_a row_lst in
      if i = move_row then List.mapi column_traversal_b lst1 else lst1
    else if i = move_row then List.mapi column_traversal_b row_lst
    else row_lst
  in
  List.mapi row_traversal st.board

let update_board st p pos = { st with board = gen_board st p pos }

let gen_falttened_board board = List.concat board

let player_turn st = st.player_turn

let update_player_turn st pt = { st with player_turn = pt }

let check st = st.check

let update_check st ch = { st with check = ch }

let checkmate st = st.checkmate

let update_checkmate st cm = { st with checkmate = cm }

let stalemate st = st.stalemate

let update_stalemate st sm = { st with stalemate = sm }

let piece_clicked st = st.piece_clicked

let update_piece_clicked st pc = { st with piece_clicked = pc }

let castle_kingside st = st.castle_kingside

let update_castle_kingside st castle =
  { st with castle_kingside = castle }

let castle_queenside st = st.castle_queenside

let update_castle_queenside st castle =
  { st with castle_queenside = castle }

let update_castle st p =
  let p_turn = st.player_turn in
  if Piece.piece_type p = King then
    if p_turn = 1 then
      update_castle_queenside
        (update_castle_kingside st (false, snd st.castle_kingside))
        (false, snd st.castle_queenside)
    else
      update_castle_queenside
        (update_castle_kingside st (fst st.castle_kingside, false))
        (fst st.castle_queenside, false)
  else if Piece.piece_type p = Rook then
    let rook_pos = Piece.position p in
    let rook_col = snd rook_pos in
    if p_turn = 1 then
      if rook_col = 0 then
        update_castle_queenside st (false, snd st.castle_kingside)
      else if rook_col = 7 then
        update_castle_kingside st (false, snd st.castle_kingside)
      else st
    else if rook_col = 0 then
      update_castle_queenside st (fst st.castle_queenside, false)
    else if rook_col = 7 then
      update_castle_kingside st (fst st.castle_queenside, false)
    else st
  else st
