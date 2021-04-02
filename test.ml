open OUnit2
open Deck
open Compare
open Main
open Table

let t_player_hand =
  [ { suit = Spades; value = Three }; { suit = Spades; value = Six } ]

let t_table_flop =
  [
    { suit = Spades; value = Two };
    { suit = Spades; value = Eight };
    { suit = Spades; value = Nine };
  ]

let t_table_turn =
  [
    { suit = Spades; value = Two };
    { suit = Spades; value = Eight };
    { suit = Spades; value = Nine };
    { suit = Spades; value = Queen };
  ]

let t_table_river =
  [
    { suit = Spades; value = Two };
    { suit = Spades; value = Eight };
    { suit = Spades; value = Nine };
    { suit = Spades; value = Queen };
    { suit = Spades; value = King };
  ]

(** converts card option to string*)
let string_of_card_option card1 =
  match card1 with None -> "" | Some x -> string_of_card x

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

(** [full_deck_test] constructs an OUnit test named [name] that asserts
    with 52 the size of a deck [deck1] and the uniqueness of cards in
    it. *)
let full_deck_test (name : string) (deck1 : deck) : test =
  name >:: fun _ ->
  assert_equal 52 (size (List.sort_uniq total_compare deck1));
  assert_equal false
    (List.mem None (List.sort_uniq total_compare deck1))

(** [decks_equality_test] constructs an OUnit test named [name] that
    asserts [expected] with the sameness of the order of cards in
    [deck1] and [deck2]. *)
let decks_equality_test
    (name : string)
    (deck1 : deck)
    (deck2 : deck)
    (expected : bool) : test =
  name >:: fun _ ->
  assert_equal expected
    (String.equal
       (string_of_card_options "" deck1)
       (string_of_card_options "" deck2))

(** [create_card_test] constructs an OUnit test named [name] that
    asserts with the uniqueness of the order of cards in two shuffled
    decks. *)
let create_card_test
    (name : string)
    (expected : string)
    (valu : value)
    (sut : suit) : test =
  name >:: fun _ ->
  assert_equal expected (string_of_card (create_card valu sut))

let rec top_card_helper (deck1 : deck) (times : int) =
  if times >= 2 then top_card_helper (remove_top deck1) (times - 1)
  else top_card deck1

(** [top_card_test] constructs an OUnit test named [name] that asserts
    the quality of [top_card deck1] with [expected] after using
    [remove_top deck1] to remove [times] numbers of cards. *)
let top_card_remove_card_test
    (name : string)
    (expected : string)
    (deck1 : deck)
    (times : int) : test =
  name >:: fun _ ->
  assert_equal expected
    (string_of_card_option (top_card_helper deck1 (times + 1)))

(** [deck_test] constructs an OUnit test named [name] that asserts with
    52 the size of a deck made using [deck1] and the uniqueness of cards
    in it. *)
let size_test (name : string) (expected : int) (deck1 : deck) : test =
  name >:: fun _ -> assert_equal expected (size deck1)

let hand_of_rank_test (name : string) (value : int) (expected : string)
    : test =
  name >:: fun _ -> assert_equal expected (hand_of_rank value)

let int_of_val_test
    (name : string)
    (value : Deck.value)
    (expected : int) : test =
  name >:: fun _ -> assert_equal expected (int_of_val value)

let str_of_suit_test
    (name : string)
    (suit : Deck.suit)
    (expected : string) : test =
  name >:: fun _ -> assert_equal expected (string_of_suit suit)

let total_hand_test
    (name : string)
    (player_hand : Deck.card list)
    (table_card : Deck.card list)
    (expected : Deck.card list) : test =
  name >:: fun _ ->
  assert_equal expected (total_hand player_hand table_card)

let hand_converter_test
    (name : string)
    (hand : Deck.card list)
    (expected : card_check list) : test =
  name >:: fun _ -> assert_equal expected (hand_converter [] hand)

let hand_sort_int_test
    (name : string)
    (hand : card_check list)
    (expected : card_check list) : test =
  name >:: fun _ -> assert_equal expected (hand_sort_int hand)

(* *******END HELPER FUNCTIONS********* *)
let deck_test =
  [
    full_deck_test "Create unshuffled deck" create;
    full_deck_test "Create a shuffled deck" (shuffle create 7);
    decks_equality_test "See if two shuffled decks are different"
      (shuffle create 135) (shuffle create 2349) false;
    (*NOTE: if shuffle is truly random, there is approximately a 1/52!
      probability of this failing even if correct. Random module is
      pseudo-random, but the chance is still negligable.*)
    decks_equality_test "See if two unshuffled decks are the same"
      create create true;
    create_card_test "See if Jack of Hearts is a Jack of Hearts" "J♥"
      Jack Hearts;
    create_card_test "See if 7 of Clubs is a 7 of Clubs" "7♣" Seven
      Clubs;
    top_card_remove_card_test "See if top of unshuffled deck is correct"
      "A♣" create 0;
    top_card_remove_card_test
      "See if a few down of unshuffled deck is correct" "J♣" create 3;
    top_card_remove_card_test
      "See if a last in unshuffled deck is correct" "2♥" create 51;
    top_card_remove_card_test
      "See if a removing all from unshuffled deck is correct" "" create
      52;
    size_test "see if full deck is 52" 52 create;
    size_test "see if one removed from deck is 51" 51
      (remove_top create);
  ]

let table_test = []

let compare_test =
  [
    hand_of_rank_test "A valid hand" 10 "Royal Flush";
    hand_of_rank_test "A non-valid number" 342 "Error. Unknown hand!";
    int_of_val_test "A number value" Four 4;
    int_of_val_test "A royal value" Queen 12;
    str_of_suit_test "A suit" Spades "♠";
    total_hand_test "Flop" t_player_hand t_table_flop
      [
        { suit = Spades; value = Three };
        { suit = Spades; value = Six };
        { suit = Spades; value = Two };
        { suit = Spades; value = Eight };
        { suit = Spades; value = Nine };
      ];
    total_hand_test "Turn" t_player_hand t_table_turn
      [
        { suit = Spades; value = Three };
        { suit = Spades; value = Six };
        { suit = Spades; value = Two };
        { suit = Spades; value = Eight };
        { suit = Spades; value = Nine };
        { suit = Spades; value = Queen };
      ];
    total_hand_test "River" t_player_hand t_table_river
      [
        { suit = Spades; value = Three };
        { suit = Spades; value = Six };
        { suit = Spades; value = Two };
        { suit = Spades; value = Eight };
        { suit = Spades; value = Nine };
        { suit = Spades; value = Queen };
        { suit = Spades; value = King };
      ];
    (* hand_converter_test "Converting numbers" t_player_hand [ {
       string_suit = "♠"; int_value = 3 }; { string_suit = "♠";
       int_value = 6 }; ]; *)
    hand_sort_int_test "Sort numbers"
      (hand_converter [] (total_hand t_player_hand t_table_river))
      [
        { string_suit = "♠"; int_value = 2 };
        { string_suit = "♠"; int_value = 3 };
        { string_suit = "♠"; int_value = 6 };
        { string_suit = "♠"; int_value = 8 };
        { string_suit = "♠"; int_value = 9 };
        { string_suit = "♠"; int_value = 12 };
        { string_suit = "♠"; int_value = 13 };
      ];
  ]

let main_test = []

let suite =
  "test suite for A2"
  >::: List.flatten [ deck_test; table_test; compare_test; main_test ]

let _ = run_test_tt_main suite
