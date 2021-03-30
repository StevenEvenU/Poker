open OUnit2
open Deck

let create_test (name : string) : test =
  name >:: fun _ -> assert_equal 52 (size create)

let deck_test = [ create_test "Create unshuffled deck" ]

let suite = "test suite for A2" >::: List.flatten [ deck_test ]

let _ = run_test_tt_main suite
