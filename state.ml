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

let make_board (fen : string) =
  let row_lst = String.split_on_char '/' fen in
  let rec board_helper lst i =
    match lst with
    | [] -> []
    | h :: t -> string_to_lst h i 0 :: board_helper t (i + 1)
  in
  board_helper row_lst 0

let state_from_fen (fen : string) =
  {
    board = make_board fen;
    player_turn = 1;
    check = false;
    checkmate = false;
    stalemate = false;
    piece_clicked = None;
  }

let init_state () =
  (* state_from_fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR" *)
  (* state_from_fen "pppppppp/rnbqkbnr/8/8/8/8/RNBQKBNR/PPPPPPPP" *)
  (* state_from_fen "rnbqkbnr/RNBQKBNR/8/8/8/8/PPPPPPPP/pppppppp" *)
  (* state_from_fen "1n11kb1r/1NBQKBNR/r7/2qRn2/4b3/8/PPPPPPPP/pppppppp" *)
  state_from_fen "1n11kb1r/1BQKNBNR/r7/2qRn2/4b3/8/PPPPPPPP/pppppppp"

let board st = st.board

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

let update_state board pt check cm sm pc =
  {
    board;
    player_turn = pt;
    check;
    checkmate = cm;
    stalemate = sm;
    piece_clicked = pc;
  }

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
   grid. *)
let get_elt (grid : 'a list list) (loc : int * int) : 'a =
  let x = fst loc in
  let y = snd loc in
  if check_bounds grid (x, y) then List.nth (List.nth grid x) y
  else None

(* [check_empty grid clr loc] is true if the location at loc is empty
   otherwise false*)
let check_empty (grid : 'a list list) (loc : int * int) : bool =
  let p = get_elt grid loc in
  match p with Some p -> false | None -> true

(* [march st direction loc] is a list of valid locations along a given
   direction*)
let rec march st scalable clr direction loc acc =
  let i = fst direction in
  let j = snd direction in
  let x = fst loc in
  let y = snd loc in
  let x' = x + i in
  let y' = y + j in
  let loc_to_check = (x', y') in
  if not (check_bounds st.board loc_to_check) then acc
  else
    let is_empty = check_empty st.board loc_to_check in
    if is_empty then
      if scalable then
        march st scalable clr direction loc_to_check
          (loc_to_check :: acc)
      else loc_to_check :: acc
    else
      let enemy_capture =
        match get_elt st.board loc_to_check with
        | None -> false
        | Some p_other -> Piece.color p_other <> clr
      in
      if enemy_capture then loc_to_check :: acc else acc

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
  | Some p -> if String.equal clr (Piece.color p) then true else false
  | None -> false

let rec scalable_helper st p loc dir acc =
  let board = board st in
  let check_loc = (fst loc + fst dir, snd loc + snd dir) in
  if check_bounds board check_loc then
    match get_elt board check_loc with
    | Some pce ->
        if String.equal (Piece.color pce) (Piece.color p) then acc
        else scalable_helper st p check_loc dir (check_loc :: acc)
    | None -> scalable_helper st p check_loc dir (check_loc :: acc)
  else acc

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
        if snd h == 1 || snd h == -1 then
          pawn_locs_helper t scalable
            (if check_pawn_capture st clr loc h then check_loc :: acc
            else acc)
        else if fst h == 2 || fst h == -2 then
          if
            (fst h == -2 && fst loc == 6 && check_empty board check_loc)
            || fst h == 2
               && fst loc == 1
               && check_empty board check_loc
          then pawn_locs_helper t scalable (check_loc :: acc)
          else pawn_locs_helper t scalable acc
        else
          pawn_locs_helper t scalable
            (if
             (fst h == 1 && check_empty board check_loc)
             || (fst h == -1 && check_empty board check_loc)
            then check_loc :: acc
            else acc)
  in
  pawn_locs_helper
    (if String.equal clr "white" then base_moves.directions
    else List.map (fun (row, col) -> (-row, col)) base_moves.directions)
    base_moves.scalable []

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
      pr pl;
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

let move st p loc =
  if not (valid_move st p loc) then failwith "Illegal Move" else st
