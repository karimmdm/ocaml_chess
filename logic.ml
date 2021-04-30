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
  if check_bounds grid (i, j) then List.nth (List.nth grid i) j
  else None

(* [check_empty grid clr loc] is true if the location at loc is empty
   otherwise false. Precondition: loc is a valid location in grid. *)
let check_empty (grid : 'a list list) (loc : int * int) : bool =
  let p = get_elt grid loc in
  match p with Some p -> false | None -> true

(* [march st direction loc] is a list of valid locations along a given
   direction. [march] recursively checks along a certain path until the
   path is blocked by an enemy piece that can be captured or an allied
   piece. *)
let rec march st scalable clr direction loc acc =
  let loc_to_check =
    (fst loc + fst direction, snd loc + snd direction)
  in
  if not (check_bounds (State.board st) loc_to_check) then acc
  else
    let is_empty = check_empty (State.board st) loc_to_check in
    if is_empty then
      if scalable then
        march st scalable clr direction loc_to_check
          (loc_to_check :: acc)
      else loc_to_check :: acc
    else
      let enemy_capture =
        match get_elt (State.board st) loc_to_check with
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
  let board = State.board st in
  let check_loc = (fst loc + fst dir, snd loc + snd dir) in
  let p = get_elt board check_loc in
  match p with
  | Some p -> if String.equal clr (Piece.color p) then false else true
  | None -> false

let pawn_locs st p loc =
  let board = State.board st in
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
            let check_loc_prev =
              (fst check_loc - (fst h / 2), snd check_loc)
            in
            if
              fst h == -2
              && fst loc == 6
              && check_empty board check_loc
              && check_empty board check_loc_prev
              || fst h == 2
                 && fst loc == 1
                 && check_empty board check_loc
                 && check_empty board check_loc_prev
            then pawn_locs_helper t scalable (check_loc :: acc)
            else pawn_locs_helper t scalable acc
          else
            pawn_locs_helper t scalable
              (if
               (fst h == 1 && check_empty board check_loc)
               || (fst h == -1 && check_empty board check_loc)
              then check_loc :: acc
              else acc)
        else pawn_locs_helper t scalable acc
  in
  (* Reverse directions if the player is using the black pieces. *)
  pawn_locs_helper
    (if String.equal clr "white" then base_moves.directions
    else List.map (fun (row, col) -> (-row, col)) base_moves.directions)
    base_moves.scalable []

(* [is_pinned st clr pos dir] checks a given piece to see if it's pinned
   down. If it's pinned down, then we restrict its valid moves *)
let is_pinned st clr pos dir = failwith "TODO: optional optimization"
(* let dir_to_check = (-fst dir, -snd dir) in let march_locs = march st
   true clr dir pos [] in let rec check_marc_locs lst = match lst with |
   [] -> false | h::t -> false *)

(* [find_king clr grid] returns the King in that grid that matches that
   [clr]. *)
let rec find_king clr grid =
  (* [find_king_in_row row] searches the row and returns the [clr] King
     if it is found and None otherwise. *)
  let rec find_king_in_row row =
    match row with
    | [] -> None
    | h :: t -> (
        match h with
        | None -> find_king_in_row t
        | Some p ->
            if
              Piece.piece_type p = Piece.King
              && String.equal (Piece.color p) clr
            then Some p
            else find_king_in_row t)
  in
  match grid with
  | [] -> failwith "King not found on board"
  | h :: t -> (
      match find_king_in_row h with
      | Some p -> p
      | None -> find_king clr t)

let rec scan_for_enemy st scalable loc dir clr piece_type_lst =
  let loc_to_check = (fst loc + fst dir, snd loc + snd dir) in
  if not (check_bounds (State.board st) loc_to_check) then false
  else
    match get_elt (State.board st) loc_to_check with
    | None ->
        if scalable then
          scan_for_enemy st scalable loc_to_check dir clr piece_type_lst
        else false
    | Some p ->
        if
          String.equal (Piece.color p) clr
          && List.mem (Piece.piece_type p) piece_type_lst
        then true
        else false

let threat st scalable king dirs piece_type_lst =
  let loc = Piece.position king in
  let enemy_clr =
    if Piece.color king <> "white" then "white" else "black"
  in
  let rec threat_helper lst =
    match lst with
    | [] -> false
    | h :: t ->
        if scan_for_enemy st scalable loc h enemy_clr piece_type_lst
        then true
        else threat_helper t
  in
  threat_helper dirs

let is_check st =
  let board = State.board st in
  let player_turn = State.player_turn st in
  let king_clr = if player_turn = 1 then "white" else "black" in
  let king = find_king king_clr board in
  let diag_threat =
    threat st true king
      [ (1, 1); (-1, 1); (1, -1); (-1, -1) ]
      [ Piece.Bishop; Piece.Queen ]
  in
  let pawn_threat =
    threat st false king
      (if king_clr = "white" then [ (-1, 1); (-1, -1) ]
      else [ (1, 1); (1, -1) ])
      [ Piece.Pawn ]
  in
  let vert_threat =
    threat st true king [ (-1, 0); (1, 0) ] [ Piece.Queen; Piece.Rook ]
  in
  let hor_threat =
    threat st true king [ (0, 1); (0, -1) ] [ Piece.Queen; Piece.Rook ]
  in
  diag_threat || vert_threat || hor_threat || pawn_threat

(* [switch_turn st] returns a new State switching the appropriate fields
   when the player turn changes. This new state updates the player turn,
   updates the piece clicked, and updates the fen. *)
let switch_turn st =
  let player_turn_st =
    State.update_player_turn st
      (if State.player_turn st == 1 then 2 else 1)
  in
  let pc_st = State.update_piece_clicked player_turn_st None in
  pc_st

(* let sample_move_piece st p new_pos = let move_st = State.update_board
   st p new_pos in let switch_turn_st = switch_turn move_st in
   switch_turn_st *)
let move_piece st p new_pos =
  let move_st = State.update_board st p new_pos in
  let check_st = State.update_check move_st (is_check move_st) in
  let switch_turn_st = switch_turn check_st in
  switch_turn_st

(* [filter_check st p locs acc] filters out [locs], a list of valid
   moves for [p] that causes the current player's king to be in check. *)
let rec filter_check st p locs acc =
  match locs with
  | [] -> acc
  | h :: t ->
      let sample_st = move_piece st p h in
      if State.check sample_st then filter_check st p t acc
      else filter_check st p t (h :: acc)

let locations st p =
  let locs_helper st p loc =
    let clr = Piece.color p in
    let base_moves = Piece.base_moves (Piece.piece_type p) in
    let scalable = base_moves.scalable in
    let rec helper acc = function
      | [] -> acc
      | h :: t -> march st scalable clr h loc [] @ helper acc t
    in
    helper [] base_moves.directions
  in
  let piece = Piece.piece_type p in
  match piece with
  | Pawn ->
      let pawn_moves = pawn_locs st p (Piece.position p) in
      filter_check st p pawn_moves []
  | Bishop ->
      let bishop_moves = locs_helper st p (Piece.position p) in
      filter_check st p bishop_moves []
  | Knight ->
      let knight_moves = locs_helper st p (Piece.position p) in
      (* pr pl; *)
      filter_check st p knight_moves []
  | Rook ->
      let rook_moves = locs_helper st p (Piece.position p) in
      (* pr pl; *)
      filter_check st p rook_moves []
  | Queen ->
      let queen_moves = locs_helper st p (Piece.position p) in
      (* pr pl; *)
      filter_check st p queen_moves []
  | King ->
      let king_moves = locs_helper st p (Piece.position p) in
      (* pr pl; *)
      filter_check st p king_moves []

(* 1n11kb1r/1BQKNBNR/r7/2qRn3/4b2P/8/PPPPPPP1/pppppppp *)
(* let invert_fen st = let rec rev_string str ind = if ind >=
   String.length str then "" else rev_string str (ind + 1) ^ String.make
   1 str.[ind] in let rec rev_section str ind = let fen_substr =
   String.sub str 0 ind in rev_string fen_substr 0 in let rec
   build_invert_fen fen ind acc = try let paren_ind = String.index_from
   fen ind '/' in let fen_len = String.length fen in let rev_str =
   rev_section fen (paren_ind - 1) in let rev_str_len = String.length
   rev_str in print_endline fen; print_endline (string_of_int (paren_ind
   + 1)); print_endline (string_of_int (fen_len - rev_str_len - 2));
   print_endline (string_of_int fen_len); let fen_str = String.sub fen
   (paren_ind + 1) (fen_len - rev_str_len - 2) in build_invert_fen
   fen_str (paren_ind + 1) (acc ^ rev_str ^ "/") with Not_found ->
   String.sub acc 0 (String.length acc - 1) in let fen = State.to_fen st
   in let final = build_invert_fen fen 0 "" in print_endline fen;
   print_endline final; final *)
(* let fen = ref (State.to_fen st) in let new_fen = ref "" in let
   paren_exists = ref true in while !paren_exists do try let fen_paren =
   String.index !fen '/' in !new_fen ^ rev_fen_section (String.sub !fen
   0 (fen_paren - 1)) ^ "/"; fen := String.sub !fen (fen_paren + 1)
   (String.length !fen - fen_paren + 1); () with Not_found ->
   paren_exists := false; () done; let final = rev_string !new_fen 0 in
   print_endline !fen; print_endline final; final *)

let valid_move st piece loc = List.mem loc (locations st piece)
