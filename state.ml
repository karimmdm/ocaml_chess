open Piece

type t = {
  board : Piece.t option list list;
  player_turn : int;
  check : bool;
  checkmate : bool;
  stalemate : bool;
  (* can_castle : bool list; *)
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
      if none_count = 0 then Piece.to_string p ^ lst_to_string t 0
      else
        string_of_int none_count ^ Piece.to_string p ^ lst_to_string t 0
  | None :: t -> lst_to_string t (none_count + 1)

let fen_to_board (str : string) =
  let row_lst = String.split_on_char '/' str in
  let rec board_helper lst i =
    match lst with
    | [] -> []
    | h :: t -> string_to_lst h i 0 :: board_helper t (i + 1)
  in
  board_helper row_lst 0

let get_status_flags (str : string) =
  let flags_lst = String.split_on_char ',' str in
  match flags_lst with
  | [] -> failwith "flag statuses needed"
  | [ pt; c; cm; sm ] -> ((pt, c), (cm, sm))
  | h :: t -> failwith "flag statuses were not appropriately entered"

let state_from_fen fen st_option =
  let fen_split_lst = String.split_on_char ':' fen in
  let board_str = List.hd fen_split_lst in
  let new_board = fen_to_board board_str in
  match st_option with
  | None ->
      let flag_status_str = List.hd (List.tl fen_split_lst) in
      let flag_statuses = get_status_flags flag_status_str in
      (* player_turn, check*)
      let flag_pt_c = fst flag_statuses in
      (*checkmate, stalemate*)
      let flag_cm_sm = snd flag_statuses in
      let bool_of_tf = function
        | "t" -> true
        | "f" -> false
        | _ -> failwith "not t or f"
      in
      {
        board = new_board;
        player_turn = int_of_string (fst flag_pt_c);
        check = bool_of_tf (snd flag_pt_c);
        checkmate = bool_of_tf (fst flag_cm_sm);
        stalemate = bool_of_tf (snd flag_cm_sm);
        piece_clicked = None;
      }
  | Some st -> { st with board = new_board }

let rec board_to_fen board =
  match board with
  | [] -> ""
  | [ h ] -> lst_to_string h 0
  | h :: t -> lst_to_string h 0 ^ "/" ^ board_to_fen t

let to_fen t = board_to_fen t.board

let init_state () =
  state_from_fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR:1,f,f,f"
    None
(* state_from_fen
   "1p1k4/pPpPpPPP/P1P1P1P1/8/8/8/PPPPPPPP/RNBQKBNR:1,f,f,f" None *)

(* state_from_fen
   "rnbqkbnr/pppp2pp/8/4pp2/3PP3/BP6/P1P2PPP/RN1QKBNR:1,f,f,f" None *)
(* state_from_fen "RNBQKBNR/PPPPPPPP/8/8/8/8/pppppppp/rnbqkbnr:1,f,f,f"
   None *)

(* state_from_fen "pppppppp/rnbqkbnr/8/8/8/8/RNBQKBNR/PPPPPPPP:1,f,f,f" *)
(* state_from_fen "rnbqkbnr/RNBQKBNR/8/8/8/8/PPPPPPPP/pppppppp:1,f,f,f" *)
(* state_from_fen
   "1n11kb1r/1NBQKBNR/r7/2qRn2/4b3/8/PPPPPPPP/pppppppp:1,f,f,f" *)
(* state_from_fen
   "1n11kb1r/1BQKNBNR/r7/2qRn3/4b2P/8/PPPPPPP1/pppppppp:1,f,f,f" None *)

let board st = st.board

(* [gen_board st pos] returns a new board that reflects
   [st.piece_clicked] moving to location [pos] on the board. requires:
   [st.piece_clicked] to be Some piece [pos] to be the position of a
   valid move location for [st.piece_clicked]*)
let gen_board st move_to_pos =
  let move_row = fst move_to_pos in
  let move_col = snd move_to_pos in
  let piece_clicked_pos =
    match st.piece_clicked with
    | None ->
        failwith
          "getting piece clicked pos: gen_board should not be called \
           if piece_clicked is none"
    | Some piece -> Piece.position piece
  in
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
      let pc =
        match st.piece_clicked with
        | None ->
            failwith "piece_clicked should not be none when moving"
        | Some p -> p
      in
      if j = move_col then
        Some (Piece.update_position pc (move_row, move_col))
      else elt
    in
    if i = piece_clicked_row then
      let lst1 = List.map column_traversal_a row_lst in
      if i = move_row then List.mapi column_traversal_b lst1 else lst1
    else if i = move_row then List.mapi column_traversal_b row_lst
    else row_lst
  in
  List.mapi row_traversal st.board

let update_board st p pos = { st with board = gen_board st pos }

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
