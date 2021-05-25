(**[check_bounds grid loc] returns true if the given location is within
   the bounds of the given grid and false otherwise. *)
let check_bounds (grid : 'a option list list) (loc : int * int) : bool =
  let width = List.length grid in
  let height = List.length (List.hd grid) in
  let row = fst loc in
  let col = snd loc in
  if row >= width || row < 0 || col >= height || col < 0 then false
  else true

(**[get_elt grid x y] returns an option of element in the grid at (x, y)
   if that element exists and if x and y are within the bounds of the
   grid. Precondition: (x, y) is a valid location in grid. *)
let get_elt (grid : 'a list list) (loc : int * int) : 'a =
  let i = fst loc in
  let j = snd loc in
  if check_bounds grid (i, j) then List.nth (List.nth grid i) j
  else None

(**[check_empty grid clr loc] is true if the location at loc is empty
   otherwise false. Precondition: loc is a valid location in grid. *)
let check_empty (grid : 'a list list) (loc : int * int) : bool =
  let p = get_elt grid loc in
  match p with Some p -> false | None -> true

(**[enemy_capture clr board loc_to_check] returns true if [loc_to_check]
  is empty on the given [board] and false if an enemy piece (not [clr]) occupies 
  that location. *)
let enemy_capture clr board loc_to_check =
  match get_elt board loc_to_check with
  | None -> false
  | Some p_other -> Piece.color p_other <> clr

(**[march st direction loc] is a list of valid locations along a given
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
    else if enemy_capture clr (State.board st) loc_to_check then
      loc_to_check :: acc
    else acc

let pawn_locs st p loc =
  let board = State.board st in
  let clr = Piece.color p in
  let row = if clr = "white" then 6 else 1 in
  let dir = if clr = "white" then -1 else 1 in
  let forward_one =
    if check_empty board (fst loc + dir, snd loc) then
      [ (fst loc + dir, snd loc) ]
    else []
  in
  let forward_two =
    if fst loc = row && check_empty board (fst loc + (2 * dir), snd loc)
    then [ (fst loc + (2 * dir), snd loc) ]
    else []
  in
  let left_attack =
    if enemy_capture clr board (fst loc + dir, snd loc - 1) then
      [ (fst loc + dir, snd loc - 1) ]
    else []
  in
  let right_attack =
    if enemy_capture clr board (fst loc + dir, snd loc + 1) then
      [ (fst loc + dir, snd loc + 1) ]
    else []
  in
  forward_one @ forward_two @ left_attack @ right_attack

(**[find_pieces clr grid] returns the King in that grid that matches
   that [clr]. *)
let rec find_pieces clr piece_type grid acc =
  (**[find_pieces_in_row row acc ] searches the row and returns the
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
            else find_pieces_in_row t acc )
  in
  match grid with
  | [] -> acc
  | h :: t -> find_pieces clr piece_type t (find_pieces_in_row h acc)

(**[check_side_castle st side] returns true if the king and [side] rook
   are valid to castle on the specified [side] and if there are no
   pieces blocking the path, and false otherwise. *)
let check_side_castle st side =
  let p_turn = State.player_turn st in
  let board = State.board st in
  let castle_side_pair =
    if side = "king" then State.castle_kingside st
    else State.castle_queenside st
  in
  let castle_side =
    if p_turn = 1 then fst castle_side_pair else snd castle_side_pair
  in
  let is_check = State.check st in
  let squares =
    if side = "king" then
      if p_turn = 1 then [ (7, 5); (7, 6) ] else [ (0, 5); (0, 6) ]
    else if p_turn = 1 then [ (7, 1); (7, 2); (7, 3) ]
    else [ (0, 1); (0, 2); (0, 3) ]
  in
  List.fold_left
    (fun acc x -> acc && get_elt board x = None)
    true squares
  && castle_side && not is_check

(**[castle_side_move st side locs] checks the current State's
  castle_kingside or castle_queenside pair based on [side] and adds the 
  corresponding castling location to the list of valid locations for the king 
  if the king can castle that way. *)
let castle_side_move st side locs =
  let p_turn = State.player_turn st in
  let castle_side_pair =
    if side = "king" then State.castle_kingside st
    else State.castle_queenside st
  in
  let new_loc =
    if side = "king" then 
      if p_turn = 1 then (7, 6) else (0, 6)
  else
    if p_turn = 1 then (7, 2) else (0, 2)
  in
  if fst castle_side_pair && check_side_castle st side then new_loc :: locs
  else locs

(**[castle_move st p new_pos] checks if the king is castling by
   comparing the king's position to the new position. If the king is
   castling, then the king is moved along with the kingside or queenside
   rook. *)
let castle_move st king new_pos =
  let board = State.board st in
  let new_king_col = snd new_pos in
  let rook_pos = (fst new_pos, if new_king_col = 6 then 7 else 0) in
  let new_rook_pos =
    (fst rook_pos, if new_king_col = 6 then 5 else 3)
  in
  let rook =
    match get_elt board rook_pos with
    | None -> failwith "Rook must exist"
    | Some r -> r
  in
  let move_king_st = State.update_board st king new_pos in
  State.update_board move_king_st rook new_rook_pos

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

(**[is_check st] returns true if the current player's king is in check
   given the board in [st], and false otherwise. *)
let is_check st =
  let board = State.board st in
  let player_turn = State.player_turn st in
  let king_clr = if player_turn = 1 then "white" else "black" in
  let king = List.hd (find_pieces king_clr King board []) in
  let diag_threat =
    threat st true king (Piece.base_moves Piece.Bishop).directions
      [ Bishop; Queen ]
  in
  let pawn_threat =
    threat st false king
      ( if king_clr = "white" then [ (-1, 1); (-1, -1) ]
      else [ (1, 1); (1, -1) ] )
      [ Pawn ]
  in
  let king_threat =
    threat st false king (Piece.base_moves Piece.King).directions
      [ King ]
  in
  let knight_threat =
    threat st false king (Piece.base_moves Piece.Knight).directions
      [ Knight ]
  in
  let vert_threat =
    threat st true king [ (-1, 0); (1, 0) ] [ Queen; Rook ]
  in
  let hor_threat =
    threat st true king [ (0, 1); (0, -1) ] [ Queen; Rook ]
  in
  diag_threat || vert_threat || hor_threat || pawn_threat
  || knight_threat || king_threat

(**[sample_move_piece st p new_pos] returns a new State that reflects
   [p] moving to [new_pos] in the given board. This state is an example
   of what would happen if the given move was made. *)
let sample_move_piece st p new_pos =
  let move_st = State.update_board st p new_pos in
  let check_st = State.update_check move_st (is_check move_st) in
  check_st

(**[filter_illegal_moves st p locs acc] filters out [locs], a list of
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

(**[locs_helper st p loc] returns a list of all valid moves for the piece [p]
  at location [loc]. *)
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
      let pawn_moves = pawn_locs st p (Piece.position p) in
      filter_illegal_moves st p pawn_moves []
  | Bishop ->
      let bishop_moves = locs_helper st p (Piece.position p) in
      filter_illegal_moves st p bishop_moves []
  | Knight ->
      let knight_moves = locs_helper st p (Piece.position p) in
      filter_illegal_moves st p knight_moves []
  | Rook ->
      let rook_moves = locs_helper st p (Piece.position p) in
      filter_illegal_moves st p rook_moves []
  | Queen ->
      let queen_moves = locs_helper st p (Piece.position p) in
      filter_illegal_moves st p queen_moves []
  | King ->
      let king_moves = locs_helper st p (Piece.position p) in
      castle_side_move st "king" (filter_illegal_moves st p king_moves [])
      @ castle_side_move st "queen"
          (filter_illegal_moves st p king_moves [])

(**[enemy_check st piece_moved] returns true if [piece_moved] has moved
  to its new location where it now puts the enemy king in check, and false
  otherwise. *)
let enemy_check st piece_moved =
  let new_locs = locations st piece_moved in
  let enemy_clr =
    if State.player_turn st = 1 then "black" else "white"
  in
  let board = State.board st in
  let enemy_king = List.hd (find_pieces enemy_clr King board []) in
  let enemy_king_pos = Piece.position enemy_king in
  List.mem enemy_king_pos new_locs

(**[find_allied_pieces clr grid] returns a list of the [clr] player's pieces. *)
let find_allied_pieces clr grid =
  find_pieces clr King grid []
  @ find_pieces clr Queen grid []
  @ find_pieces clr Bishop grid []
  @ find_pieces clr King grid []
  @ find_pieces clr Knight grid []
  @ find_pieces clr Pawn grid []

(**[is_mate st clr mate_type] determines if the [clr] player is in either
  checkmate or stalemate, specified by [mate_type] by determining if they
  are in check and have any valid moves left, or if they have any valid moves 
  left. *)
let is_mate st clr mate_type =
  let rec find_locs_helper lst acc =
    match lst with
    | [] -> acc
    | h :: t -> find_locs_helper t (locations st h @ acc)
  in
  let board = State.board st in
  let allied_moves =
    find_locs_helper (find_allied_pieces clr board) []
  in
  let moves_left = List.length allied_moves in
  if mate_type = "checkmate" then is_check st && moves_left = 0
  else moves_left = 0


let is_checkmate st clr = is_mate st clr "checkmate"

let is_stalemate st clr = is_mate st clr "stalemate"

(**[switch_turn st] returns a new State switching the player turn. *)
let switch_turn st =
  State.update_player_turn st
    (if State.player_turn st == 1 then 2 else 1)

(**[reset_piece_clicked st] sets the current state's pieced click field to 
  None. *)
let reset_piece_clicked st = State.update_piece_clicked st None

let move_piece st p new_pos =
  let castle_st = State.update_castle st p in
  let is_castle =
    Piece.piece_type p = King
    && abs (snd (Piece.position p) - snd new_pos) > 1
  in
  let move_st =
    if is_castle then castle_move st p new_pos
    else State.update_board castle_st p new_pos
  in
  let switch_turn_st = switch_turn move_st in
  let check_st =
    State.update_check switch_turn_st (is_check switch_turn_st)
  in
  let clr = if State.player_turn st = 1 then "black" else "white" in
  let checkmate_st =
    State.update_checkmate check_st (is_checkmate check_st clr)
  in
  let stalemate_st =
    State.update_stalemate checkmate_st (is_stalemate checkmate_st clr)
  in
  let reset_st = reset_piece_clicked stalemate_st in
  reset_st

let valid_move st piece loc = List.mem loc (locations st piece)
