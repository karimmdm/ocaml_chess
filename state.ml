open Piece

type t = {
  board : Piece.t option list list;
  player_turn : int;
  check : bool;
  checkmate : bool;
  stalemate : bool;
  piece_clicked : Piece.t option;
  fen: string
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
    fen = fen
  }

let init_state () =
  (* state_from_fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR" *)
  (* state_from_fen "pppppppp/rnbqkbnr/8/8/8/8/RNBQKBNR/PPPPPPPP" *)
  (* state_from_fen "rnbqkbnr/RNBQKBNR/8/8/8/8/PPPPPPPP/pppppppp" *)
  state_from_fen "1n11kb1r/1NBQKBNR/r7/2qRn2/4b3/11111P1p/PPPPPPPP/pppppppp"

let board st = st.board

let player_turn st = st.player_turn

let update_player_turn st pt = {st with player_turn = pt}

let check st = st.check

let update_check st ch = {st with check = ch}

let checkmate st = st.checkmate

let update_checkmate st cm = {st with checkmate = cm}

let stalemate st = st.stalemate

let update_stalemate st sm = {st with stalemate = sm}

let piece_clicked st = st.piece_clicked

let update_piece_clicked st pc = {st with piece_clicked = pc}

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
  let x = fst loc in
  let y = snd loc in
  (* if check_bounds grid (x, y) then List.nth (List.nth grid x) y
  else None *)
  List.nth (List.nth grid x) y

(* [check_empty grid clr loc] is true if the location at loc is empty
   otherwise false*)
let check_empty (grid : 'a list list) (loc : int * int) : bool =
  if check_bounds grid loc then
    let p = get_elt grid loc in
    match p with Some p -> false | None -> true
  else false

(* [march st direction loc] is a list of valid locations along a given
   direction. [march] recursively checks along a certain path until the path
   is blocked by an enemy piece that can be captured or an allied piece. *)
let rec march st clr direction loc acc =
  let i = fst direction in
  let j = snd direction in
  let x = fst loc in
  let y = snd loc in
  let x' = x + i in
  let y' = y + j in
  (* let loc_to_check = (x', y') in *)
  let loc_to_check = (fst direction + fst loc, snd direction + snd loc) in
  if not (check_bounds st.board loc_to_check) then acc
  else
    (* let is_empty = check_empty st.board loc_to_check in *)
    (* if is_empty then *)
    if check_empty st.board loc_to_check then
      march st clr direction loc_to_check (loc_to_check :: acc)
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

let bishop_locs st p loc =
  let clr = Piece.color p in
  let base_moves = Piece.base_moves (Piece.piece_type p) in
  let rec bishop_helper acc = function
    | [] -> acc
    | h :: t -> march st clr h loc [] @ bishop_helper acc t
  in
  bishop_helper [] base_moves.directions

let knight_locs st p loc =
  let clr = Piece.color p in
  let base_moves = Piece.base_moves (Piece.piece_type p) in
  let rec knight_helper lst acc =
    match lst with
    | [] -> acc
    | h :: t ->
        if not (check_bounds st.board loc) then knight_helper t acc
        else
          let loc_to_check = (fst h + fst loc, snd h + snd loc) in
          if not (check_bounds st.board loc_to_check) then
            knight_helper t acc
          else
            let is_empty = check_empty st.board loc_to_check in
            let enemy_capture =
              match get_elt st.board loc_to_check with
              | None -> false
              | Some p_other -> Piece.color p_other <> clr
            in
            if is_empty || enemy_capture then
              knight_helper t (loc_to_check :: acc)
            else knight_helper t acc
  in
  knight_helper base_moves.directions []

let rook_locs st p loc =
  let clr = Piece.color p in
  let base_moves = Piece.base_moves (Piece.piece_type p) in
  let rec rook_helper acc = function
    | [] -> acc
    | h :: t -> march st clr h loc [] @ rook_helper acc t
  in
  rook_helper [] base_moves.directions

let locations st p =
  let piece = Piece.piece_type p in
  match piece with
  | Pawn ->
      let pl = pawn_locs st p (Piece.position p) in
      (* pr pl; *)
      pl
  | Bishop ->
      let pl = bishop_locs st p (Piece.position p) in
      (* pr pl; *)
      pl
  | Knight ->
      let pl = knight_locs st p (Piece.position p) in
      (* pr pl; *)
      pl
  | Rook ->
      let pl = rook_locs st p (Piece.position p) in
      (* pr pl; *)
      pl
  | Queen -> []
  | King -> []

let valid_move st p loc = List.mem loc (locations st p)

let move_piece st p pos =
  (* "1n11kb1r/1NBQKBNR/r7/2qRn2/4b3/11111P1p/PPPPPPPP/pppppppp" *)
  let fen = "" in
  {st with board = make_board fen}

(* let move st p loc =
  if not (valid_move st p loc) then failwith "Illegal Move" else st *)
