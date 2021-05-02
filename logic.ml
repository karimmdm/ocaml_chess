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
  | [] ->
      print_endline;
      print_endline "End of valid possible move locations"
  | h :: t ->
      print_string
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

(* [find_pieces clr grid] returns the King in that grid that matches
   that [clr]. *)
let rec find_pieces clr piece_type grid acc =
  (* [find_pieces_in_row row acc ] searches the row and returns the
     [clr] pieces if it is found and None otherwise. *)
  let rec find_pieces_in_row row acc =
    match row with
    | [] -> acc
    | h :: t -> (
        match h with
        | None -> find_pieces_in_row t acc
        | Some p ->
            if Piece.piece_type p = piece_type && Piece.color p = clr
            then find_pieces_in_row t (p :: acc)
            else find_pieces_in_row t acc)
  in
  match grid with
  | [] -> acc
  | h :: t -> find_pieces clr piece_type t (find_pieces_in_row h acc)

let rec scan_for_enemy st scalable loc dir clr piece_type_lst =
  let loc_to_check = (fst loc + fst dir, snd loc + snd dir) in
  let board = State.board st in
  if not (check_bounds board loc_to_check) then false
  else
    match get_elt board loc_to_check with
    | None ->
        if scalable then
          scan_for_enemy st scalable loc_to_check dir clr piece_type_lst
        else false
    | Some p ->
        if
          Piece.color p = clr
          && List.mem (Piece.piece_type p) piece_type_lst
        then true
        else false

let threat st scalable king dirs piece_type_lst =
  let king_pos = Piece.position king in
  let enemy_clr =
    if Piece.color king <> "white" then "white" else "black"
  in
  let rec threat_helper lst =
    match lst with
    | [] -> false
    | h :: t ->
        if
          scan_for_enemy st scalable king_pos h enemy_clr piece_type_lst
        then true
        else threat_helper t
  in
  threat_helper dirs

(* [is_check st] returns true if the current player's king is in check
   given the board in [st], and false otherwise. *)
let is_check st =
  let board = State.board st in
  let player_turn = State.player_turn st in
  let king_clr = if player_turn = 1 then "white" else "black" in
  let king = List.hd (find_pieces king_clr King board []) in
  let diag_threat =
    threat st true king
      [ (1, 1); (-1, 1); (1, -1); (-1, -1) ]
      [ Bishop; Queen ]
  in
  let pawn_threat =
    threat st false king
      (if king_clr = "white" then [ (-1, 1); (-1, -1) ]
      else [ (1, 1); (1, -1) ])
      [ Pawn ]
  in
  let knight_threat =
    threat st false king
      [
        (2, 1);
        (2, -1);
        (-2, 1);
        (-2, -1);
        (1, 2);
        (1, -2);
        (-1, 2);
        (-1, -2);
      ]
      [ Knight ]
  in
  let vert_threat =
    threat st true king [ (-1, 0); (1, 0) ] [ Queen; Rook ]
  in
  let hor_threat =
    threat st true king [ (0, 1); (0, -1) ] [ Queen; Rook ]
  in
  diag_threat || vert_threat || hor_threat || pawn_threat
  || knight_threat

(* [sample_move_piece st p new_pos] returns a new State that reflects
   [p] moving to [new_pos] in the given board. This state is an example
   of what would happen if the given move was made. *)
let sample_move_piece st p new_pos =
  let move_st = State.update_board st p new_pos in
  let check_st = State.update_check move_st (is_check move_st) in
  check_st

(* [filter_illegal_moves st p locs acc] filters out [locs], a list of
   valid moves for [p], removing moves that cause the current player's
   king to be in check. *)
let rec filter_illegal_moves st p locs acc =
  match locs with
  | [] -> acc
  | h :: t ->
      let sample_st =
        sample_move_piece (State.update_piece_clicked st (Some p)) p h
      in
      if State.check sample_st then filter_illegal_moves st p t acc
      else filter_illegal_moves st p t (h :: acc)

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
      (* pr (filter_check st p pawn_moves []); *)
      filter_illegal_moves st p pawn_moves []
  | Bishop ->
      let bishop_moves = locs_helper st p (Piece.position p) in
      (* pr (filter_check st p bishop_moves []); *)
      filter_illegal_moves st p bishop_moves []
  | Knight ->
      let knight_moves = locs_helper st p (Piece.position p) in
      (* pr (filter_check st p knight_moves []); *)
      filter_illegal_moves st p knight_moves []
  | Rook ->
      let rook_moves = locs_helper st p (Piece.position p) in
      (* pr (filter_check st p rook_moves []); *)
      filter_illegal_moves st p rook_moves []
  | Queen ->
      let queen_moves = locs_helper st p (Piece.position p) in
      (* pr (filter_check st p queen_moves []); *)
      filter_illegal_moves st p queen_moves []
  | King ->
      let king_moves = locs_helper st p (Piece.position p) in
      (* pr (filter_check st p king_moves []); *)
      filter_illegal_moves st p king_moves []

let enemy_check st piece_moved =
  let new_locs = locations st piece_moved in
  let enemy_clr =
    if State.player_turn st = 1 then "black" else "white"
  in
  let board = State.board st in
  let enemy_king = List.hd (find_pieces enemy_clr King board []) in
  let enemy_king_pos = Piece.position enemy_king in
  List.mem enemy_king_pos new_locs

let find_allied_pieces clr grid =
  find_pieces clr King grid []
  @ find_pieces clr Queen grid []
  @ find_pieces clr Bishop grid []
  @ find_pieces clr King grid []
  @ find_pieces clr Knight grid []
  @ find_pieces clr Pawn grid []

let is_checkmate st clr =
  let rec helper lst acc =
    match lst with
    | [] -> acc
    | h :: t ->
        (* print_string (Printer.print_piece h ^ ": "); *)
        (* pr (locations st h); *)
        helper t (locations st h @ acc)
  in
  let board = State.board st in
  let allied_moves = helper (find_allied_pieces clr board) [] in
  let moves_left = List.length allied_moves in
  (* is_check st && moves_left = 0 *)
  moves_left = 0

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

let move_piece st p new_pos =
  let move_st = State.update_board st p new_pos in
  (* let check_st = State.update_check move_st (is_check move_st) in *)
  let switch_turn_st = switch_turn move_st in
  let clr = if State.player_turn st = 1 then "black" else "white" in
  let checkmate_st =
    State.update_checkmate switch_turn_st
      (is_checkmate switch_turn_st clr)
  in
  checkmate_st

let valid_move st piece loc = List.mem loc (locations st piece)
