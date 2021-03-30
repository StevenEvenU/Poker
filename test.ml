open OUnit2
open Deck
open Compare

(** Maps suits to multiples of 100 to assist [total_compare]*)
let int_of_suit suit =
  match suit with
  | Spades -> 0
  | Hearts -> 100
  | Diamonds -> 200
  | Clubs -> 300

(** Compare cards to create sorted unique list*)
let total_compare (fst : card option) (snd : card option) =
  let fst_num =
    match fst with
    | None -> 0
    | Some x -> int_of_suit x.suit + int_of_val x.value
  in
  let snd_num =
    match snd with
    | None -> 0
    | Some x -> int_of_suit x.suit + int_of_val x.value
  in
  compare fst_num snd_num

(** [deck_test] constructs an OUnit test named [name] that asserts with
    52 the size of a deck made using [deck1] and the uniqueness of cards
    in it. *)
let deck_test (name : string) (deck1 : deck) : test =
  name >:: fun _ ->
  assert_equal 52 (size (List.sort_uniq total_compare deck1));
  assert_equal false
    (List.mem None (List.sort_uniq total_compare deck1))

let deck_test =
  [
    deck_test "Create unshuffled deck" create;
    deck_test "Create a shuffled decks" (shuffle create);
  ]

let suite = "test suite for A2" >::: List.flatten [ deck_test ]

let _ = run_test_tt_main suite
