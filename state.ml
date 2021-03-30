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

(* [check_main_diag grid clr x y acc] returns a list of positions along
  the main diagonal that are empty or a valid capture. *)
(* let rec check_main_diag grid clr x y acc =
  if check_bounds grid (x, y) then 
    let elt = get_elt grid x y in
      match elt with
      | Some p -> 
          if not (String.equal (Piece.color p) clr) then (x, y)::acc
          else acc
      | None -> 
        if String.equal clr "white" then
          check_main_diag grid clr (x + 1) (y + 1) ((x, y)::acc) @
          check_main_diag grid clr (x - 1) (y - 1) ((x, y)::acc)
        else 
          check_main_diag grid clr (x + 1) (y - 1) ((x, y)::acc) @
          check_main_diag grid clr (x - 1) (y + 1) ((x, y)::acc)
  else [] *)

(* [check_diag grid clr x y acc diag] returns a list of positions along
  the diagonal specified by [diag] that are empty or a valid capture. *)
let rec check_diag grid clr x y acc diag =
  if check_bounds grid (x, y) then 
    let elt = get_elt grid x y in
      match elt with
      | Some p -> 
          if not (String.equal (Piece.color p) clr) then (x, y)::acc
          else acc
      | None -> 
        if String.equal clr "white" then
          if String.equal diag "main" then 
            check_diag grid clr (x + 1) (y + 1) ((x, y)::acc) diag @
            check_diag grid clr (x - 1) (y - 1) ((x, y)::acc) diag
          else
            check_diag grid clr (x - 1) (y + 1) ((x, y)::acc) diag @
            check_diag grid clr (x + 1) (y - 1) ((x, y)::acc) diag
        else 
          if String.equal diag "main" then 
            check_diag grid clr (x + 1) (y - 1) ((x, y)::acc) diag @
            check_diag grid clr (x - 1) (y + 1) ((x, y)::acc) diag
          else 
            check_diag grid clr (x - 1) (y - 1) ((x, y)::acc) diag @
            check_diag grid clr (x + 1) (y + 1) ((x, y)::acc) diag
  else acc

(* [check_snd_diag grid clr x y acc] returns a list of positions along
  the secondary diagonal that are empty or a valid capture. *)
(* let rec check_snd_diag grid clr x y acc =
  if check_bounds grid (x, y) then 
    let elt = get_elt grid x y in
      match elt with
      | Some p ->
        if not (String.equal (Piece.color p) clr) then (x, y)::acc
        else acc
      | None ->
        if String.equal clr "white" then
          check_snd_diag grid clr (x - 1) (y + 1) ((x, y)::acc) @
          check_snd_diag grid clr (x + 1) (y - 1) ((x, y)::acc)
        else 
          check_snd_diag grid clr (x - 1) (y - 1) ((x, y)::acc) @
          check_snd_diag grid clr (x + 1) (y + 1) ((x, y)::acc)
  else [] *)

let rec check_rank board clr x y acc dir = 
  if check_bounds board (x, y) then
    let elt = get_elt board x y in
      match elt with
      | Some p ->
        if not (String.equal (Piece.color p) clr) then ((x, y)::acc)
        else acc
      | None -> 
        if String.equal dir "vertical" then
          check_rank board clr x (y + 1) dir @ 
          check_rank board clr x (y - 1) dir
        else
          check_rank board clr (x + 1) y dir @ 
          check_rank board clr (x - 1) y dir
        (* match dir with
        | "vertical" -> 
            check_rank board clr x (y + 1) dir
            (* check_rank board clr x (y + 1) dir @ 
            check_rank board clr x (y - 1) dir *)
        | "horizontal" ->
            (check_rank board clr (x + 1) y dir) @ 
            (check_rank board clr (x - 1) y dir)
        | _ -> [] *)
  else acc

let locations st p =
  let x = fst (Piece.position p) in
  let y = snd (Piece.position p) in
  let board = st.board in
  let clr = Piece.color p in
  let piece = Piece.piece_type p in
  match piece with
  | Pawn ->
    (* [pawn_capture board clr x y] returns a list of positions that the given 
      pawn piece can move to after a legal capture. *)
    let pawn_capture board clr x y =
      if String.equal clr "white" then
        let check_locs = (x + 1, y + 1)::(x - 1, y + 1)::[] in
        let rec helper lst acc = 
          match lst with
          | h::t -> helper t (if check_bounds board h then h::acc else acc)
          | [] -> acc in
        helper check_locs []
      else
        let check_locs = (x + 1, y - 1)::(x - 1, y - 1)::[] in
        let rec helper lst acc = 
          match lst with
          | h::t -> helper t (if check_bounds board h then h::acc else acc)
          | [] -> acc in
        helper check_locs [] in
    (* [pawn_capture board clr x y] returns a list of positions that the given 
      pawn piece can move to, excluding captures. *)
    let pawn_move board clr x y =
      if String.equal clr "white" then
        if y == 1 then [(x, y + 1); (x, y + 2)]
        else if check_bounds board (x, y + 1) then [(x, y + 1)] else []
      else [] in
    (pawn_capture board clr x y) @ (pawn_move board clr x y)
  | Bishop ->
    (* [bishop_move board clr x y] returns a list of positions that the given
      bishop piece can move to, including captures. *)
    let bishop_move board clr x y =
      (check_diag board clr x y [] "main") @ 
      (check_diag board clr x y [] "snd") in
    bishop_move board clr x y
  | Knight ->
    (* [knight_move board clr x y] returns a list of positions that the given
      knight piece can move to, including captures. *)
    let knight_move board clr x y = 
      (* [valid_positions_helper lst acc] returns [acc] which contains all the  
        valid board positions in [lst]. *)
      let rec valid_positions_helper lst acc =
        match lst with
        | h::t -> 
          if check_bounds board h then 
            let elt = get_elt board x y in
            match elt with
            | Some p -> 
              if not (String.equal (Piece.color p) clr) then 
                valid_positions_helper t (h::acc)
              else valid_positions_helper t acc
            | None -> valid_positions_helper t (h::acc)
          else valid_positions_helper t acc
        | [] -> acc in
      let check_locs = 
        [(x + 1, y + 2); (x - 1, y + 2); (x + 2, y + 1); (x - 2, y + 1);
         (x + 1, y - 2); (x - 1, y - 2); (x + 2, y - 1); (x - 2, y - 1)] in
      valid_positions_helper check_locs [] in 
    knight_move board clr x y
  | Rook ->
    (* [knight_move board clr x y] returns a list of positions that the given
      knight piece can move to, including captures. *)
    let rook_move board clr x y = 
      let rec check_hor_right board clr x y (acc : (int * int) list) = 
        if check_bounds board (x, y) then
          let elt = get_elt board x y in
            match elt with
            | Some p ->
              if not (String.equal (Piece.color p) clr) then ((x, y)::acc)
              else acc
            | None -> check_hor_right board clr (x + 1) y ((x, y)::acc)
        else [] in
      let rec check_hor_left board clr x y (acc : (int * int) list) = 
        if check_bounds board (x, y) then
          let elt = get_elt board x y in
            match elt with
            | Some p ->
              if not (String.equal (Piece.color p) clr) then ((x, y)::acc)
              else acc
            | None -> check_hor_left board clr (x - 1) y ((x, y)::acc)
        else [] in
      let rec check_ver_up board clr x y (acc : (int * int) list) = 
        if check_bounds board (x, y) then
          let elt = get_elt board x y in
            match elt with
            | Some p ->
              if not (String.equal (Piece.color p) clr) then ((x, y)::acc)
              else acc
            | None -> check_ver_up board clr x (y + 1) ((x, y)::acc)
        else [] in
      let rec check_ver_down board clr x y (acc : (int * int) list) = 
        if check_bounds board (x, y) then
          let elt = get_elt board x y in
            match elt with
            | Some p ->
              if not (String.equal (Piece.color p) clr) then ((x, y)::acc)
              else acc
            | None -> check_ver_down board clr x (y - 1) ((x, y)::acc)
        else [] in
      (check_hor_right board clr x y []) @ (check_hor_left board clr x y []) @ 
      (check_ver_up board clr x y []) @ (check_ver_down board clr x y []) in
    rook_move board clr x y
  | Queen -> []
  | King -> []

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
