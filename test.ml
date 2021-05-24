open OUnit2
open Piece
open State
open Printer
open Gui

(*TEST PLAN*)
(* We decided to test changes in the GUI manually because we were able
   to visually ensure that the GUI behaved as expected. We tested by
   checking if the GUI board was being made properly with each of the
   pairs and that it correctly updated with each players move. We also
   tested the functionality of the server manually through Postman. We
   made sure the json file was updating properly with each change. We
   decided this was the best method of testing to handle all the
   requests in the backend. On the other hand, we decided to test the
   piece, state, and logic modules by OUnit because they were not as
   visible from the GUI and to make sure all aspects were entirely
   accounted for.BLACK BOX???????????*)

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

let state_test (name : string) (expected : string) (state : State.t) =
  name >:: fun _ ->
  assert_equal expected (Printer.print_board state) ~printer:(fun x ->
      x)

let to_fen_test (name : string) (expected : string) (state : State.t) =
  name >:: fun _ ->
  assert_equal expected (State.to_fen state) ~printer:(fun x -> x)

let state_kingside_castle_test = failwith ""

let state_queenside_castle_test = failwith ""

let logic_locations_test = failwith ""

let logic_valid_move_test = failwith ""

let logic_is_check_test = failwith ""

let logic_is_checkmate_test = failwith ""

let logic_is_stalemate_test = failwith ""

let logic_move_piece_test = failwith ""

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
    (* state_test "empty" empty_board_string (init_state ()); *)
    (* state_test "starting" starting_string (state_from_fen
       "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR:1,f,f,f" None);
       state_test "e4" starting_string_e4 (state_from_fen
       "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR:1,f,f,f" None);
       to_fen_test "starting board to fen"
       "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR" (state_from_fen
       "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR:1,f,f,f" None); *)
    ( "hi" >:: fun _ ->
      assert_equal
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR:1,false,false,false,true;true,true;true"
        (State.to_fen (init_state ()))
        ~printer:(fun x -> x) );
  ]

let logic_tests = []

let suite =
  "test suite for chess game"
  >::: List.flatten [ piece_tests; state_tests; logic_tests ]

let _ = run_test_tt_main suite
