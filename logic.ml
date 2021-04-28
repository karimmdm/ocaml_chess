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
  let i = fst direction in
  let j = snd direction in
  let x = fst loc in
  let y = snd loc in
  let x' = x + i in
  let y' = y + j in
  let loc_to_check = (x', y') in
  if not (check_bounds (State.board st) loc_to_check) then acc
    (* let sample_state = move_to st loc_to_check if sample_state.check
       = true then acc else --> *)
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
      (* pr pl; *)
      pl
  | Knight ->
      let pl = locs_helper st p (Piece.position p) in
      (* pr pl; *)
      pl
  | Rook ->
      let pl = locs_helper st p (Piece.position p) in
      (* pr pl; *)
      pl
  | Queen ->
      let pl = locs_helper st p (Piece.position p) in
      (* pr pl; *)
      pl
  | King ->
      let pl = locs_helper st p (Piece.position p) in
      (* pr pl; *)
      pl

let valid_move st piece loc = List.mem loc (locations st piece)

let switch_turn st =
  let player_turn_st =
    State.update_player_turn st
      (if State.player_turn st == 1 then 2 else 1)
  in
  let pc_st = State.update_piece_clicked player_turn_st None in
  pc_st

let move_piece st p new_pos =
  let move_st = State.update_board st p new_pos in
  move_st
(* switch_turn move_st *)
