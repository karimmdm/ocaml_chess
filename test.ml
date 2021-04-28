open OUnit2
open Piece
open State
open Printer
open Gui

(********************************************************************
   Here are some helper functions for your testing. 
 ********************************************************************)
let init_state_test name expected =
  name >:: fun _ -> assert_equal expected (State.board (init_state ()))

let piece_test
    (name : string)
    (expected : string)
    (c : char)
    (posn : int * int) =
  name >:: fun _ ->
  let p = make c posn in
  assert_equal expected (print_piece p) ~printer:(fun x -> x)

let state_test (name : string) (expected : string) (state : State.t) =
  name >:: fun _ ->
  assert_equal expected (Printer.print_board state) ~printer:(fun x ->
      x)

let to_fen_test (name : string) (expected : string) (state : State.t) =
  name >:: fun _ ->
  assert_equal expected (State.to_fen state) ~printer:(fun x -> x)

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
    piece_test "test Wlack pawn in 0,0 should be BP (0,0)" "BP" 'p'
      (0, 0);
    piece_test "test white king in 7,3 should be WK (7,3)" "WK" 'K'
      (7, 3);
  ]

let state_tests =
  [
    (* state_test "empty" empty_board_string (init_state ()); *)
    state_test "starting" starting_string
      (state_from_fen
         "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR:1,f,f,f");
    state_test "e4" starting_string_e4
      (state_from_fen
         "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR:1,f,f,f");
    to_fen_test "starting board to fen"
      "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
      (state_from_fen
         "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR:1,f,f,f");
  ]

let gui_tests = []

let suite =
  "test suite for A2"
  >::: List.flatten [ piece_tests; state_tests; gui_tests ]

let _ = run_test_tt_main suite
