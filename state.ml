open Piece

type t = {
  board : Piece.t option list list;
  player_turn : int;
  check : bool;
  checkmate : bool;
  stalemate : bool;
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
  }

let init_state () =
  state_from_fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

let board st = st.board

let player_turn st = st.player_turn

let check st = st.check

let checkmate st = st.checkmate

let stalemate st = st.stalemate

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

let check_empty (grid : 'a list list) (clr : string) (loc : int * int) :
    bool =
  let p = get_elt grid loc in
  match p with Some p -> false | None -> true

let rec pr l =
  match l with
  | [] -> print_endline "End"
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
            fst h == -2
            && fst loc == 6
            && check_empty board clr check_loc
            || fst h == 2
               && fst loc == 1
               && check_empty board clr check_loc
          then pawn_locs_helper t scalable (check_loc :: acc)
          else pawn_locs_helper t scalable acc
        else
          pawn_locs_helper t scalable
            (if
             (fst h == 1 && check_empty board clr check_loc)
             || (fst h == -1 && check_empty board clr check_loc)
            then check_loc :: acc
            else acc)
  in
  pawn_locs_helper
    (if String.equal clr "white" then base_moves.directions
    else List.map (fun (row, col) -> (-row, col)) base_moves.directions)
    base_moves.scalable []

let bishop_locs st p loc =
  let base_moves = Piece.base_moves (Piece.piece_type p) in
  let rec bishop_locs_helper lst scalable acc =
    match lst with
    | [] -> acc
    | h :: t ->
        bishop_locs_helper t scalable (scalable_helper st p loc h acc)
  in
  bishop_locs_helper base_moves.directions base_moves.scalable []

let locations st p =
  let piece = Piece.piece_type p in
  match piece with
  | Pawn ->
      let pl = pawn_locs st p (Piece.position p) in
      pr pl;
      pl
  | Bishop ->
      let pl = bishop_locs st p (Piece.position p) in
      pr pl;
      pl
  | Knight -> []
  | Rook -> []
  | Queen -> []
  | King -> []

let valid_move st p loc = List.mem loc (locations st p)
