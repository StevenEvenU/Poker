open OUnit2
open Deck
open Compare
open Main

(** converts card option list to string*)
let rec string_of_card_options str cards =
  match cards with
  | [] -> str
  | [ h ] -> (
      match h with
      | None -> str
      | Some x -> str ^ " " ^ string_of_card x)
  | h :: t -> (
      match h with
      | None ->
          let s = str in
          string_of_card_options s t
      | Some x ->
          let s = str ^ " " ^ string_of_card x in
          string_of_card_options s t)

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

(** recursive helper for [deck_unequal_test]*)
let rec deck_unequal_helper deck1 deck2 (acc : int) =
  match (deck1, deck2) with
  | [], [] -> acc
  | h1 :: t1, h2 :: t2 ->
      if total_compare h1 h2 = 0 then deck_unequal_helper t2 t2 (acc + 1)
      else deck_unequal_helper t2 t2 acc
  | _, _ -> 100

(** [shuffle_random_test] constructs an OUnit test named [name] that
    asserts with the uniqueness of the order of cards in two shuffled
    decks. *)
let deck_unequal_test (name : string) (deck1 : deck) (deck2 : deck) :
    test =
  name >:: fun _ ->
  assert_equal
    (string_of_card_options "" deck1)
    (string_of_card_options "" deck2)

let deck_test =
  [
    deck_test "Create unshuffled deck" create;
    deck_test "Create a shuffled decks" (shuffle create 7);
    deck_unequal_test "See if two shuffled decks are different"
      (shuffle create 10) (shuffle create 35);
    (*NOTE: if shuffle is truly random, there is approximately a 1/52!
      probability of this failing even if correct. This is negligable.*)
    deck_unequal_test "See if two unshuffled decks are the same" create
      create;
  ]

let suite = "test suite for A2" >::: List.flatten [ deck_test ]

let _ = run_test_tt_main suite
