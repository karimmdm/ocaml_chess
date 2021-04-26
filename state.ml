open Piece

type t = {
  board : Piece.t option list list;
  player_turn : int;
  check : bool;
  checkmate : bool;
  stalemate : bool;
  piece_clicked : Piece.t option;
}

let letter_to_piece_type c : piece =
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

let player_turn st = st.player_turn

let update_player_turn st pt = { st with player_turn = pt }

let check st = st.check

let update_check st ch = { st with check = ch }

let checkmate st = st.checkmate

let update_checkmate st cm = { st with checkmate = cm }

let stalemate st = st.stalemate

let update_stalemate st sm = { st with stalemate = sm }

(* let piece_clicked st = st.piece_clicked

   let update_piece_clicked st pc = { st with piece_clicked = pc } *)

let update_state board pt check cm sm pc =
  { board; player_turn = pt; check; checkmate = cm; stalemate = sm }

(* [check_bounds grid loc] returns true if the given location is within
   the bounds of the given grid and false otherwise. *)
let check_bounds (grid : 'a option list list) (loc : int * int) : bool =
  let width = List.length grid in
  let height = List.length (List.hd grid) in
  let row = fst loc in
  let col = snd loc in
  if row >= width || row < 0 || col >= height || col < 0 then false
  else true

(* [get_elt grid x y] returns an option of element in the grid at (x, y)
   if that element exists and if x and y are within the bounds of the
   grid. Precondition: (x, y) is a valid location in grid. *)
let get_elt (grid : 'a list list) (loc : int * int) : 'a =
  let i = fst loc in
  let j = snd loc in
  print_endline "Nth tracker";
  print_endline (string_of_int i);
  print_endline (string_of_int j);
  print_endline "End of Nth tracker";
  print_string "testing ith row, length: ";
  print_endline (string_of_int (List.length (List.nth grid i)));
  if check_bounds grid (i, j) then List.nth (List.nth grid i) j
  else None

(* [check_empty grid clr loc] is true if the location at loc is empty
   otherwise false. Precondition: loc is a valid location in grid. *)
let check_empty (grid : 'a list list) (loc : int * int) : bool =
  print_endline ((string_of_int (fst loc)) ^ " " ^ (string_of_int (snd loc)) ^ " -> " ^ string_of_bool (check_bounds grid loc));
  let p = get_elt grid loc in
  match p with Some p -> false | None -> true

(* [march st direction loc] is a list of valid locations along a given
   direction. [march] recursively checks along a certain path until the path
   is blocked by an enemy piece that can be captured or an allied piece. *)
let rec march st scalable clr direction loc acc =
  let i = fst direction in
  let j = snd direction in
  let x = fst loc in
  let y = snd loc in
  let x' = x + i in
  let y' = y + j in
  let loc_to_check = (x', y') in
  if not (check_bounds st.board loc_to_check) then acc
  (* let sample_state = move_to st loc_to_check
     if sample_state.check = true then acc else --> *)
  else
    let is_empty = check_empty st.board check_loc in
    if is_empty then
      if scalable then
        march st scalable clr direction loc_to_check
          (loc_to_check :: acc)
      else loc_to_check :: acc
    else
      let enemy_capture =
        match get_elt st.board check_loc with
        | None -> false
        | Some p_other -> Piece.color p_other <> clr
      in
      if enemy_capture then check_loc :: acc else acc

let rec pr l =
  match l with
  | [] -> print_endline "End of valid possible move locations"
  | h :: t ->
      print_endline
        ("("
        ^ string_of_int (fst h)
        ^ ", "
        ^ string_of_int (snd h)
        ^ ")");
      pr t

let check_pawn_capture st clr loc dir =
  let board = board st in
  let check_loc = (fst loc + fst dir, snd loc + snd dir) in
  let p = get_elt board check_loc in
  match p with
  | Some p -> if String.equal clr (Piece.color p) then false else true
  | None -> false

let pawn_locs st p loc =
  let board = board st in
  let clr = Piece.color p in
  let base_moves = Piece.base_moves (Piece.piece_type p) in
  let rec pawn_locs_helper lst scalable acc =
    match lst with
    | [] -> acc
    | h :: t ->
        (* Check if the direction involves moving to the right or left
           column. If so, then this location direciton is a piece
           capture. *)
        let check_loc = (fst h + fst loc, snd h + snd loc) in
        if check_bounds board check_loc then
          if snd h == 1 || snd h == -1 then
            pawn_locs_helper t scalable
              (if check_pawn_capture st clr loc h then check_loc :: acc
              else acc)
          else if fst h == 2 || fst h == -2 then
            let check_loc_prev = (fst check_loc - (fst h)/2, snd check_loc) in
            if
              (fst h == -2 && fst loc == 6 && check_empty board check_loc && check_empty board check_loc_prev)
              || fst h == 2
                && fst loc == 1
                && check_empty board check_loc && check_empty board check_loc_prev
            then pawn_locs_helper t scalable (check_loc :: acc)
            else pawn_locs_helper t scalable acc
          else
            pawn_locs_helper t scalable
              (if
              (fst h == 1 && check_empty board check_loc)
              || (fst h == -1 && check_empty board check_loc)
              then check_loc :: acc
              else acc)
        else
          pawn_locs_helper t scalable acc
  in
  (* Reverse directions if the player is using the black pieces. *)
  pawn_locs_helper
    (if String.equal clr "white" then base_moves.directions
    else List.map (fun (row, col) -> (-row, col)) base_moves.directions)
    base_moves.scalable []

let rook_locs st p loc =
  let clr = Piece.color p in
  let base_moves = Piece.base_moves (Piece.piece_type p) in
  let rec rook_helper acc = function
let locs_helper st p loc =
  let clr = Piece.color p in
  let base_moves = Piece.base_moves (Piece.piece_type p) in
  let scalable = base_moves.scalable in
  let rec helper acc = function
    | [] -> acc
    | h :: t -> march st scalable clr h loc [] @ helper acc t
  in
  helper [] base_moves.directions

let locations st p =
  let piece = Piece.piece_type p in
  match piece with
  | Pawn ->
      let pl = pawn_locs st p (Piece.position p) in
      (* pr pl; *)
      pl
  | Bishop ->
      let pl = locs_helper st p (Piece.position p) in
      pr pl;
      pl
  | Knight ->
      let pl = locs_helper st p (Piece.position p) in
      pr pl;
      pl
  | Rook ->
      let pl = locs_helper st p (Piece.position p) in
      pr pl;
      pl
  | Queen ->
      let pl = locs_helper st p (Piece.position p) in
      pr pl;
      pl
  | King ->
      let pl = locs_helper st p (Piece.position p) in
      pr pl;
      pl

let valid_move st p loc = List.mem loc (locations st p)

let move_piece st p pos =
  (* "1n11kb1r/1NBQKBNR/r7/2qRn2/4b3/11111P1p/PPPPPPPP/pppppppp" *)
  let fen = "" in
  {st with board = make_board fen}

(* let move st p loc =
  if not (valid_move st p loc) then failwith "Illegal Move" else st *)
