open OUnit2
open Piece
open State
open Printer
open Gui

(*TEST PLAN*)
(**We decided to test changes in the GUI manually because we were able
   to visually ensure that the GUI behaved as expected. We tested by
   checking if the GUI board was being made properly with each of the
   pairs and that it correctly updated with each players move. We also
   tested the functionality of the server manually through Postman. We
   made sure the json file was updating properly with each change. We
   decided this was the best method of testing to handle all the
   requests in the backend. On the other hand, we decided to test the
   piece, state, and logic modules by OUnit because they were not as
   visible from the GUI and to make sure all aspects were entirely
   accounted for. Certain logical parts were tested using glass box
   methoding due to the limited possibilities and paths. We also used
   black box testing when testing checks and checkmates to test for
   corner cases. The test cases ensured everything was correct by
   checking all features of the game are correct at every time with
   every move.*)

let piece_base_moves_test
    (name : string)
    (expected : string)
    (p : Piece.piece) =
  name >:: fun _ ->
  assert_equal expected
    (p |> Piece.base_moves |> Printer.print_rule)
    ~printer:(fun x -> x)

let piece_to_letter_test
    (name : string)
    (expected : string)
    (c : char)
    (pos : int * int) =
  name >:: fun _ ->
  let p = make c pos in
  assert_equal expected (Piece.to_letter p) ~printer:(fun x -> x)

let state_to_board (name : string) (expected : string) (fen : string) =
  name >:: fun _ ->
  assert_equal expected
    (State.state_from_fen fen None |> Printer.print_board)
    ~printer:(fun x -> x)

let state_update_castle_test
    (name : string)
    (expected : string)
    (fen : string)
    (c : char)
    (pos : int * int) =
  name >:: fun _ ->
  assert_equal expected
    (Piece.make c pos
    |> State.update_castle (State.state_from_fen fen None)
    |> State.to_fen)
    ~printer:(fun x -> x)

let to_fen_test (name : string) (expected : string) (state : State.t) =
  name >:: fun _ ->
  assert_equal expected (State.to_fen state) ~printer:(fun x -> x)

(**let state_kingside_castle_test = failwith ""

   let state_queenside_castle_test = failwith "" *)

let logic_locations_test
    (name : string)
    (expected : string)
    (fen : string)
    (c : char)
    (pos : int * int) =
  name >:: fun _ ->
  assert_equal expected
    (Printer.print_locs
       (Logic.locations
          (State.state_from_fen fen None)
          (Piece.make c pos)))
    ~printer:(fun x -> x)

let logic_is_check_test (name : string) (expected : bool) (fen : string)
    =
  name >:: fun _ ->
  assert_equal expected
    (State.state_from_fen fen None |> Logic.is_check)
    ~printer:string_of_bool

let logic_is_mate_test
    (name : string)
    (expected : bool)
    (mate : State.t -> string -> bool)
    (clr : string)
    (fen : string) =
  name >:: fun _ ->
  assert_equal expected
    (mate (State.state_from_fen fen None) clr)
    ~printer:string_of_bool

let logic_valid_move_test
    (name : string)
    (expected : bool)
    (fen : string)
    (c : char)
    (pos : int * int)
    (loc : int * int) =
  name >:: fun _ ->
  assert_equal expected
    (Logic.valid_move
       (State.state_from_fen fen None)
       (Piece.make c pos) loc)
    ~printer:string_of_bool

let logic_move_piece_test
    (name : string)
    (expected : string)
    (fen : string)
    (c : char)
    (pos : int * int)
    (loc : int * int) =
  name >:: fun _ ->
  assert_equal expected
    (Logic.move_piece
       (State.state_from_fen fen None)
       (Piece.make c pos) loc
    |> State.to_fen)
    ~printer:(fun x -> x)

let init_state_fen =
  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR:1,false,false,false,true;true,true;true"

let empty_board_string =
  "\n\
   |  |  |  |  |  |  |  |  |\n\
   |  |  |  |  |  |  |  |  |\n\
   |  |  |  |  |  |  |  |  |\n\
   |  |  |  |  |  |  |  |  |\n\
   |  |  |  |  |  |  |  |  |\n\
   |  |  |  |  |  |  |  |  |\n\
   |  |  |  |  |  |  |  |  |\n\
   |  |  |  |  |  |  |  |  |"

let starting_string =
  "\n\
   |BR|BN|BB|BQ|BK|BB|BN|BR|\n\
   |BP|BP|BP|BP|BP|BP|BP|BP|\n\
   |  |  |  |  |  |  |  |  |\n\
   |  |  |  |  |  |  |  |  |\n\
   |  |  |  |  |  |  |  |  |\n\
   |  |  |  |  |  |  |  |  |\n\
   |WP|WP|WP|WP|WP|WP|WP|WP|\n\
   |WR|WN|WB|WQ|WK|WB|WN|WR|"

let starting_string_e4 =
  "\n\
   |BR|BN|BB|BQ|BK|BB|BN|BR|\n\
   |BP|BP|BP|BP|BP|BP|BP|BP|\n\
   |  |  |  |  |  |  |  |  |\n\
   |  |  |  |  |  |  |  |  |\n\
   |  |  |  |  |WP|  |  |  |\n\
   |  |  |  |  |  |  |  |  |\n\
   |WP|WP|WP|WP|  |WP|WP|WP|\n\
   |WR|WN|WB|WQ|WK|WB|WN|WR|"

(********************************************************************
  End helper functions.
  ********************************************************************)

let piece_tests =
  [
    piece_base_moves_test "Base moves of pawn"
      "[-1,0][-2,0][-1,1][-1,-1] false" Pawn;
    piece_base_moves_test "Base moves of knight"
      "[-2,-1][-2,1][2,-1][2,1][-1,-2][-1,2][1,-2][1,2] false" Knight;
    piece_base_moves_test "Base moves of bishop"
      "[-1,-1][-1,1][1,-1][1,1] true" Bishop;
    piece_base_moves_test "Base moves of rook"
      "[-1,0][1,0][0,1][0,-1] true" Rook;
    piece_base_moves_test "Base moves of queen"
      "[-1,-1][-1,1][1,-1][1,1][1,0][-1,0][0,1][0,-1] true" Queen;
    piece_base_moves_test "Base moves of king"
      "[-1,-1][-1,0][-1,1][0,-1][0,1][1,-1][1,0][1,1] false" King;
    piece_to_letter_test "Letter for white pawn" "P" 'P' (2, 4);
    piece_to_letter_test "Letter for white knight" "N" 'N' (2, 4);
    piece_to_letter_test "Letter for black bishop" "b" 'b' (2, 4);
    piece_to_letter_test "Letter for white queen" "Q" 'Q' (2, 4);
    piece_to_letter_test "Letter for black king" "k" 'k' (2, 4);
    piece_to_letter_test "Letter for white rook" "R" 'R' (2, 4);
  ]

let state_tests =
  [
    state_to_board "empty board" empty_board_string
      "8/8/8/8/8/8/8/8:1,false,false,false,true;true,true;true";
    state_to_board "init board" starting_string init_state_fen;
    state_to_board "starting e3" starting_string_e4
      "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR:1,false,false,false,true;true,true;true";
    state_update_castle_test "update moving p1 king"
      "rnbqkbnr/ppp1pppp/8/3p4/3P4/8/PPP1PPPP/RNBQKBNR:1,false,false,false,false;true,false;true"
      "rnbqkbnr/ppp1pppp/8/3p4/3P4/8/PPP1PPPP/RNBQKBNR:1,false,false,false,true;true,true;true"
      'K' (7, 4);
    state_update_castle_test "update moving p2 king"
      "rnbqkbnr/ppp1pppp/8/3p4/3P4/8/PPP1PPPP/RNBQKBNR:2,false,false,false,true;false,true;false"
      "rnbqkbnr/ppp1pppp/8/3p4/3P4/8/PPP1PPPP/RNBQKBNR:2,false,false,false,true;true,true;true"
      'k' (0, 4);
    state_update_castle_test "update moving p1 rook"
      "rnbqkbnr/1ppppppp/8/p7/P7/R7/1PPPPPPP/1NBQKBNR:1,false,false,false,true;true,false;true"
      "rnbqkbnr/1ppppppp/8/p7/P7/R7/1PPPPPPP/1NBQKBNR:1,false,false,false,true;true,true;true"
      'R' (7, 0);
    state_update_castle_test "update moving p2 rook"
      "1nbqkbnr/rppppppp/8/p7/P7/R7/1PPPPPPP/1NBQKBNR:2,false,false,false,true;true,true;false"
      "1nbqkbnr/rppppppp/8/p7/P7/R7/1PPPPPPP/1NBQKBNR:2,false,false,false,true;true,true;true"
      'r' (1, 0);
  ]

let logic_tests =
  [
    logic_locations_test "test white pawn first move locations"
      "[(5,4)(4,4)]"
      "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR:1,false,false,false,true;true,true;true"
      'P' (6, 4);
    logic_locations_test
      "test black pawn second move locations no attack" "[(3,4)]"
      "rnbqkbnr/pppp1ppp/4p3/8/8/8/PPPPPPPP/RNBQKBNR:2,false,false,false,true;true,true;true"
      'p' (2, 4);
    logic_locations_test "test white pawns locations attack"
      "[(3,4)(3,3)(3,5)]"
      "rnbqkbnr/ppp3pp/4p3/3p1p2/4P3/8/PPPP1PPP/RNBQKBNR:1,false,false,false,true;true,true;true"
      'P' (4, 4);
    logic_locations_test "test white pawns locations only left"
      "[(3,4)(3,3)]"
      "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR:1,false,false,false,true;true,true;true"
      'P' (4, 4);
    logic_locations_test "test white pawns locations blocked" "[]"
      "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR:1,false,false,false,true;true,true;true"
      'P' (4, 4);
    logic_locations_test "test king that is in check" "[(1,4)(1,4)]"
      "rnbqkbnr/ppp2ppp/8/1B1pp3/3PP3/8/PPP2PPP/RNBQK1NR:2,false,false,false,true;true,true;true"
      'k' (0, 4);
    logic_locations_test "test king that is in checkmate" "[]"
      "rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR:1,true,true,false,false;false,false;true"
      'K' (7, 4);
    logic_locations_test "test knight moves all open"
      "[(1,3)(1,5)(5,3)(5,5)(2,2)(2,6)(4,2)(4,6)]"
      "k7/8/8/4N3/8/8/8/7K:1,false,false,false,true;true,true;true" 'N'
      (3, 4);
    logic_locations_test "test bishop moves all open"
      "[(0,1)(1,2)(2,3)(0,7)(1,6)(2,5)(7,0)(6,1)(5,2)(4,3)(6,7)(5,6)(4,5)]"
      "k7/8/8/4B3/8/8/8/7K:1,false,false,false,true;true,true;true" 'B'
      (3, 4);
    logic_locations_test "test bishop all blocked" "[]"
      "k7/8/3KNR2/4B3/3KQR2/8/8/7R:1,false,false,false,true;true,true;true"
      'B' (3, 4);
    logic_locations_test "test rook moves all open"
      "[(0,4)(1,4)(2,4)(7,4)(6,4)(5,4)(4,4)(3,7)(3,6)(3,5)(3,0)(3,1)(3,2)(3,3)]"
      "k7/8/8/4R3/8/8/8/7K:1,false,false,false,true;true,true;true" 'R'
      (3, 4);
    logic_locations_test "test queen moves all open"
      "[(0,1)(1,2)(2,3)(0,7)(1,6)(2,5)(7,0)(6,1)(5,2)(4,3)(6,7)(5,6)(4,5)(7,4)(6,4)(5,4)(4,4)(0,4)(1,4)(2,4)(3,7)(3,6)(3,5)(3,0)(3,1)(3,2)(3,3)]"
      "k7/8/8/4Q3/8/8/8/7K:1,false,false,false,true;true,true;true" 'Q'
      (3, 4);
    logic_locations_test "test king moves all open"
      "[(2,3)(2,4)(2,5)(3,3)(3,5)(4,3)(4,4)(4,5)(7,2)(2,3)(2,4)(2,5)(3,3)(3,5)(4,3)(4,4)(4,5)]"
      "k7/8/8/4K3/8/8/8/8:1,false,false,false,true;true,true;true" 'K'
      (3, 4);
    logic_is_check_test "test if white king is in check by bishop" true
      "rnbqk1nr/pppp1ppp/8/4p3/1b1P4/5P2/PPP1P1PP/RNBQKBNR:1,false,false,false,true;true,true;true";
    logic_valid_move_test "test if white can block the check" true
      "rnbqk1nr/pppp1ppp/8/4p3/1b1P4/5P2/PPP1P1PP/RNBQKBNR:1,true,false,false,true;true,true;true"
      'P' (6, 2) (5, 2);
    logic_valid_move_test "test if white can not block the check" false
      "rnbqk1nr/pppp1ppp/8/4p3/1b1P4/5P2/PPP1P1PP/RNBQKBNR:1,true,false,false,true;true,true;true"
      'P' (6, 7) (5, 7);
    logic_valid_move_test "test if white can move out of the check" true
      "rnbqk1nr/pppp1ppp/8/4p3/1b1P4/5P2/PPP1P1PP/RNBQKBNR:1,true,false,false,true;true,true;true"
      'K' (7, 4) (6, 5);
    logic_is_mate_test "test if is in checkmate; should be true" true
      Logic.is_checkmate "white"
      "8/4N1pk/8/8/8/7R/8/6K1:2,false,false,false,false;false,false;false";
    logic_is_mate_test "test if is in checkmate 2; should be true" true
      Logic.is_checkmate "white"
      "rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR:1,false,false,false,false;false,false;false";
    logic_is_mate_test "test if is in checkmate; should be false" false
      Logic.is_checkmate "white" init_state_fen;
    logic_is_mate_test
      "test if is in checkmate but in stalemate; should be false" false
      Logic.is_checkmate "white"
      "2k5/2P5/2K5/8/8/8/8/8:2,false,false,false,false;false,false;false";
    logic_is_mate_test "test if is in stalemate; should be true" true
      Logic.is_stalemate "black"
      "2k5/2P5/2K5/8/8/8/8/8:2,false,false,false,false;false,false;false";
    logic_is_mate_test "test if is in stalemate; should be false" false
      Logic.is_stalemate "white" init_state_fen;
    logic_is_mate_test
      "test if is in stalemate but in checkmate; should be true" true
      Logic.is_stalemate "white"
      "rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR:1,false,false,false,false;false,false;false";
    logic_move_piece_test "move should change board and player_turn"
      "rnbqkbnr/pppppppp/8/8/3P4/8/PPP1PPPP/RNBQKBNR:2,false,false,false,true;true,true;true"
      init_state_fen 'P' (6, 3) (4, 3);
    logic_move_piece_test "move should change board and player_turn"
      "rnbqkbnr/pppp1ppp/8/4p3/3P4/8/PPP1PPPP/RNBQKBNR:1,false,false,false,true;true,true;true"
      "rnbqkbnr/pppppppp/8/8/3P4/8/PPP1PPPP/RNBQKBNR:2,false,false,false,true;true,true;true"
      'p' (1, 4) (3, 4);
    logic_move_piece_test "move capture test"
      "rnbqkbnr/pppp1ppp/8/4P3/8/8/PPP1PPPP/RNBQKBNR:2,false,false,false,true;true,true;true"
      "rnbqkbnr/pppp1ppp/8/4p3/3P4/8/PPP1PPPP/RNBQKBNR:1,false,false,false,true;true,true;true"
      'P' (4, 3) (3, 4);
    logic_move_piece_test "move black queenside castle test"
      "1nbqkbnr/1rpppppp/8/p7/P7/R7/1PPPPPPP/1NBQKBNR:1,false,false,false,true;true,true;false"
      "1nbqkbnr/rppppppp/8/p7/P7/R7/1PPPPPPP/1NBQKBNR:2,false,false,false,true;true,true;true"
      'r' (1, 0) (1, 1);
    logic_move_piece_test "move white queenside castle test"
      "rnbqkbnr/1ppppppp/8/p7/P7/R7/1PPPPPPP/1RBQKBNR:2,false,false,false,true;true,false;true"
      "rnbqkbnr/1ppppppp/8/p7/P7/R7/1PPPPPPP/1NBQKBNR:1,false,false,false,true;true,true;true"
      'R' (7, 0) (7, 1);
    logic_move_piece_test "move white to check"
      "rnbqkbnr/ppp2p1p/3p4/1B2P1p1/4P3/8/PPP2PPP/RNBQK1NR:2,true,false,false,true;true,true;true"
      "rnbqkbnr/ppp2p1p/3p4/4P1p1/4P3/8/PPP2PPP/RNBQKBNR:1,false,false,false,true;true,true;true"
      'B' (7, 5) (3, 1);
    logic_move_piece_test "move out of check"
      "rn1qkbnr/pppb1p1p/3p4/1B2P1p1/4P3/8/PPP2PPP/RNBQK1NR:1,false,false,false,true;true,true;true"
      "rnbqkbnr/ppp2p1p/3p4/1B2P1p1/4P3/8/PPP2PPP/RNBQK1NR:2,true,false,false,true;true,true;true"
      'b' (0, 2) (1, 3);
  ]

let suite =
  "test suite for chess game"
  >::: List.flatten [ piece_tests; state_tests; logic_tests ]

let _ = run_test_tt_main suite
