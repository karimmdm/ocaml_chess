type t = {
  board : Piece.t option list list;
  player_turn : int;
  check : bool;
  checkmate : bool;
  stalemate : bool;
}

let init_state f = {
  board = [
    [None; None; None; None; None; None; None; None];
    [None; None; None; None; None; None; None; None];
    [None; None; None; None; None; None; None; None];
    [None; None; None; None; None; None; None; None];
    [None; None; None; None; None; None; None; None];
    [None; None; None; None; None; None; None; None];
    [None; None; None; None; None; None; None; None];
    [None; None; None; None; None; None; None; None]
  ];
  player_turn = 1;
  check = false;
  checkmate = false;
  stalemate = false;
}

let player_turn st = st.player_turn

let check st = st.check

let checkmate st = st.checkmate

let stalemate st = st.stalemate

(* [check_bounds grid loc] returns true if the given location is within the
  bounds of the given grid and false otherwise. *)
let check_bounds (grid : 'a option list list) (loc : int * int) : bool =
  let width = List.length grid in
  let height = List.length (List.hd grid) in
  let x = fst loc in
  let y = snd loc in
  if x >= width || x < 0 || y >= height || y < 0 then false else true

(* [get_elt grid x y] returns an option of element in the grid at (x, y) if that
  element exists and if x and y are within the bounds of the grid. *)
let get_elt (grid : 'a list list) (x : int) (y : int) : 'a =
  if check_bounds grid (x, y) then List.nth (List.nth grid x) y
  else None

(* [is_occupied grid loc] returns true if the given location in the grid 
  contains a piece and false otherwise. *)
let is_occupied (grid : Piece.t option list list) (loc : int * int) : bool =
  let piece = get_elt grid (fst loc) (snd loc) in
  match piece with
  | Some piece -> true
  | None -> false

(* [piece_move grid loc p] returns true if the given piece can move to
    the given location in the given grid and false otherwise. *)
(* let piece_move (grid : Piece.t option list list) (p : Piece.t) 
  : (int * int) list =
     *)
(* let match_piece (piece : Piece.t option) (color : string) : bool =
  match piece with
  | Some p -> if String.equal (Piece.color p) color then false else true
  | None -> false *)

(* [can_capture p st loc] returns a list of all valid positions where the given
  piece can capture an enemy piece via official chess rules. *)
let can_capture (p : Piece.t) (st : t) : (int * int) list =
  let clr = Piece.color p in
  let x = fst (Piece.position p) in
  let y = snd (Piece.position p) in
  (* let locs = [] in *)
  match Piece.piece_type p with
  | Pawn ->
    if String.equal clr "white" then
      let check_locs = (x + 1, y + 1)::(x - 1, y + 1)::[] in
      let rec helper lst acc = 
        match lst with
        | h::t -> helper t (if check_bounds st.board h then h::acc else acc)
        | [] -> acc in
      helper check_locs []
    else
      let check_locs = (x + 1, y - 1)::(x - 1, y - 1)::[] in
      let rec helper lst acc = 
        match lst with
        | h::t -> helper t (if check_bounds st.board h then h::acc else acc)
        | [] -> acc in
      helper check_locs []
  (* | Pawn -> 
    if String.equal clr "white" then
      if l1 == x + 1 || l1 == x - 1 && l2 == y + 1 &&
        check_bounds st.board loc then let piece = get_elt st.board x y in
          match piece with
          | Some p -> 
            if String.equal (Piece.color p) "black" then loc::locs else []
          | None -> []
      else []
    else
      if l1 == x + 1 || l1 == x - 1 && l2 == y - 1 &&
        check_bounds st.board loc then let piece = get_elt st.board x y in
          match piece with
          | Some p ->
            if String.equal (Piece.color p) "white" then loc::locs else []
          | None -> []
      else [] *)
    (* if ((String.equal clr "white" && (l1 == x + 1 || l1 == x - 1) && l2 == y + 1) 
      || (l1 == x + 1 || l1 == x - 1) && l2 == y - 1) && 
        check_bounds st.board loc then
          match List.nth (List.nth st.board x) y with
          | Some piece -> 
            if not (String.equal (Piece.color piece) clr) then loc::locs else []
          | None -> []
      else [] *)
  | Bishop ->
    if String.equal clr "white" then
      let rec check_main_diag grid clr x y acc =
        if check_bounds grid (x, y) then 
          if String.equal clr "white" then 
            check_main_diag grid clr (x + 1, y + 1) (x, y)::acc else acc
          else 
            check_main_diag grid clr (x + 1, y - 1) (x, y)::acc else acc in
      let check_locs = check_diag st.board x y in
      []
      (* while check_bounds st.board l do *)
        (* let piece = get_elt st.board x y in *)
        (* let rec check_diag loc = *)
        (* if match_piece piece "white" then loc::locs else []; *)
        (* match piece with
        | Some p ->
          if String.equal (Piece.color p) "white" then loc::locs else []
        | None -> [] in *)
        (* l = (x + 1, y + 1) *)
        (* done; *)
      (* let l = (x - 1, y - 1) in
      while check_bounds st.board l do
        let piece = get_elt st.board x y in
        if match_piece piece "black" then loc::locs else []; *)
        (* match piece with
        | Some p ->
          if String.equal (Piece.color p) "white" then loc::locs else []
        | None -> [] in *)
        (* l = (x - 1, y - 1)
        done; locs *)
    else
      let l = (x - 1, y + 1) in
      while check_bounds st.board l do
        let piece = get_elt st.board x y in
        if match_piece piece "white" then loc::locs else [];
        (* match piece with
        | Some p ->
          if String.equal (Piece.color p) "white" then loc::locs else []
        | None -> [] in *)
        l = (x - 1, y + 1)
        done;
      let l = (x + 1, y - 1) in
      while check_bounds st.board l do
        let piece = get_elt st.board x y in
        if match_piece piece "black" then loc::locs else [];
        (* match piece with
        | Some p ->
          if String.equal (Piece.color p) "white" then loc::locs else []
        | None -> [] in *)
        l = (x + 1, y - 1)
        done; locs
  | Knight -> []
  | Rook -> []
  | Queen -> []
  | King -> []

let locations st p =
  let x = fst (Piece.position p) in
  let y = snd (Piece.position p) in
  let clr = Piece.color p in
  let width = st.board in
  match Piece.piece_type p with
  | Pawn -> 
      let pawn_move f = 
        let locs = [] in
        if 
      pawn_move ()
  | Bishop ->
    let bishop_move f = failwith ""
  | Knight ->
    let knight_move f = failwith ""
  | Rook -> 
    let rook_move f = failwith ""
  | Queen -> failwith ""
  | King -> failwith ""

let valid_move st (p : Piece.t) loc = 
  (* [check_bounds grid loc] returns true if the given location is within the
    bounds of the given grid and false otherwise. *)
  let check_bounds (grid : Piece.t option list list) (loc : int * int) : bool =
    let width = List.length grid in
    let height = List.length (List.hd grid) in
    let x = fst loc in
    let y = snd loc in
    if x >= width || x < 0 || y >= height || y < 0 then false else true in

  (* [is_occupied grid loc] returns true if the given location in the grid 
    contains a piece and false otherwise. *)
  let is_occupied (grid : Piece.t option list list) (loc : int * int) : bool =
    let x = fst loc in
    let y = snd loc in
    let piece = List.nth (List.nth grid x) y in
    match piece with
    | Some piece -> false
    | None -> true in

  (* [can_piece_move grid loc p] returns true if the given piece can move to
    the given location in the given grid and false otherwise. *)
  (* let can_move_piece (grid : Piece.t option list list) (loc : int * int)
    (p : Piece.t) = List.mem p. loc *)
  if check_bounds st.board loc && not (is_occupied st.board loc) then true
  else false
