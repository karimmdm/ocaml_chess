open OUnit2
open Piece
open State
open Gui

(********************************************************************
   Here are some helper functions for your testing of set-like lists. 
 ********************************************************************)
let init_state_test name expected =
  name >:: fun _ -> assert_equal expected (State.board (init_state ()))

(********************************************************************
  End helper functions.
  ********************************************************************)
let piece_tests = []

let state_tests = []

let gui_tests = []

let suite =
  "test suite for A2"
  >::: List.flatten [ piece_tests; state_tests; gui_tests ]

let _ = run_test_tt_main suite
