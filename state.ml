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

let letter_to_piece_type c =
  match c with
  | 'P' -> Pawn
  | 'B' -> Bishop
  | 'N' -> Knight
  | 'R' -> Rook
  | 'Q' -> Queen
  | _ -> King

let piece_type_to_letter piece =
  match piece with
  | Pawn -> 'P'
  | Bishop -> 'B'
  | Knight -> 'N'
  | Rook -> 'R'
  | Queen -> 'Q'
  | King -> 'K'

let letter_to_piece c pos : Piece.t =
  let color = if Char.code c - 97 < 0 then "white" else "black" in
  let piece_type = letter_to_piece_type (Char.uppercase_ascii c) in
  let prefix = if color = "white" then "w" else "b" in
  let icon_str = "./images/" ^ prefix ^ Char.escaped c ^ ".png" in
  make piece_type color icon_str pos

let rec int_to_nones i : Piece.t option list =
  if i = 0 then [] else None :: int_to_nones (i - 1)

let rec string_to_lst (s : string) (i : int) (j : int) :
    Piece.t option list =
  let length = String.length s in
  if length = 0 then []
  else
    let c = s.[0] in
    let rest = String.sub s 1 (length - 1) in
    if Char.code c < 58 then
      let n = int_of_string (Char.escaped c) in
      int_to_nones n @ string_to_lst rest i (j + n)
    else Some (letter_to_piece c (i, j)) :: string_to_lst rest i (j + 1)

let make_board (str : string) =
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

let state_from_fen (fen : string) =
  let fen_split_lst = String.split_on_char ':' fen in
  let board_str = List.hd fen_split_lst in
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
    board = make_board board_str;
    player_turn = int_of_string (fst flag_pt_c);
    check = bool_of_tf (snd flag_pt_c);
    checkmate = bool_of_tf (fst flag_cm_sm);
    stalemate = bool_of_tf (snd flag_cm_sm);
    piece_clicked = None;
  }

(* let board_to_fen board = let rec helper row = match row with | [] ->
   "/" | h :: t -> *)
let to_fen t = failwith "unimplemented"

let init_state () =
  (* state_from_fen
     "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR:1,f,f,f" *)
  (* state_from_fen
     "pppppppp/rnbqkbnr/8/8/8/8/RNBQKBNR/PPPPPPPP:1,f,f,f" *)
  (* state_from_fen
     "rnbqkbnr/RNBQKBNR/8/8/8/8/PPPPPPPP/pppppppp:1,f,f,f" *)
  (* state_from_fen
     "1n11kb1r/1NBQKBNR/r7/2qRn2/4b3/8/PPPPPPPP/pppppppp:1,f,f,f" *)
  state_from_fen
    "1n11kb1r/1BQKNBNR/r7/2qRn3/4b2P/8/PPPPPPP1/pppppppp:1,f,f,f"

let board st = st.board

(* [gen_board st pos] returns a new board that reflects piece [p] moving
   to location [pos] on the board. *)
let gen_board st p pos =
  let rec gen_row_helper acc row_lst col elt =
    match row_lst with
    | [] -> acc
    | h :: t ->
        if col == 0 then gen_row_helper (p :: acc) t (col - 1) elt
        else gen_row_helper (h :: acc) t (col - 1) elt
  in
  let rec gen_board_helper grid acc row col elt =
    match grid with [] -> acc | h :: t -> gen_row_helper [] h col elt
  in
  let orig_pos = Piece.position p in
  let orig_row = fst orig_pos in
  let orig_col = snd orig_pos in
  let new_row = fst pos in
  let new_col = snd pos in
  let board = st.board in
  (* let target_pos = Piece.pos p in let row_f = function | None -> let
     f row = List.map ( fun elem -> ) row List.map (fun r -> i) *)
  let board' = gen_board_helper board [] orig_row orig_col None in
  gen_board_helper board' [] new_row new_col p

let update_board st p pos = { st with board = gen_board st p pos }

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
