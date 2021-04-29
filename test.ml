open OUnit2
open Deck
open Compare
open Main
open Table
open State
open Pot

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

let t_state =
  {
    users_hand =
      [
        { suit = Diamonds; value = Eight };
        { suit = Hearts; value = Three };
      ];
    cpu_hands =
      [|
        [
          { suit = Clubs; value = Eight };
          { suit = Spades; value = Jack };
        ];
      |];
    cards_on_table =
      [
        { suit = Hearts; value = Two };
        { suit = Hearts; value = Six };
        { suit = Clubs; value = Ten };
        { suit = Clubs; value = Queen };
        { suit = Hearts; value = Queen };
      ];
    deck_rem =
      [
        Some { suit = Spades; value = Nine };
        Some { suit = Hearts; value = Jack };
        Some { suit = Spades; value = Six };
        Some { suit = Spades; value = King };
        Some { suit = Clubs; value = Six };
        Some { suit = Clubs; value = Two };
        Some { suit = Clubs; value = Four };
        Some { suit = Clubs; value = Nine };
        Some { suit = Diamonds; value = Ten };
        Some { suit = Diamonds; value = Three };
        Some { suit = Clubs; value = Five };
        Some { suit = Spades; value = Four };
        Some { suit = Diamonds; value = Four };
        Some { suit = Diamonds; value = Six };
        Some { suit = Hearts; value = Ten };
        Some { suit = Diamonds; value = Seven };
        Some { suit = Spades; value = Ten };
        Some { suit = Diamonds; value = Jack };
        Some { suit = Hearts; value = Five };
        Some { suit = Hearts; value = Nine };
        Some { suit = Diamonds; value = Five };
        Some { suit = Diamonds; value = Two };
        Some { suit = Diamonds; value = Ace };
        Some { suit = Hearts; value = Eight };
        Some { suit = Clubs; value = Seven };
        Some { suit = Hearts; value = Four };
        Some { suit = Diamonds; value = King };
        Some { suit = Hearts; value = Ace };
        Some { suit = Spades; value = Three };
        Some { suit = Clubs; value = King };
        Some { suit = Spades; value = Two };
        Some { suit = Spades; value = Eight };
        Some { suit = Clubs; value = Three };
        Some { suit = Diamonds; value = Queen };
        Some { suit = Hearts; value = King };
        Some { suit = Hearts; value = Seven };
        Some { suit = Spades; value = Seven };
        Some { suit = Diamonds; value = Nine };
        Some { suit = Spades; value = Queen };
        Some { suit = Spades; value = Ace };
        Some { suit = Spades; value = Five };
        Some { suit = Clubs; value = Ace };
        Some { suit = Clubs; value = Jack };
      ];
    turn = Player;
    user_money = 1000;
    cpu_moneys = Array.make 2 1000;
    dealer = Player;
    current_bet = 0;
  }

let t_state_1 =
  {
    users_hand =
      [
        { suit = Diamonds; value = Eight };
        { suit = Hearts; value = Three };
      ];
    cpu_hands =
      [|
        [
          { suit = Clubs; value = Eight };
          { suit = Spades; value = Jack };
        ];
      |];
    cards_on_table =
      [
        { suit = Hearts; value = Two };
        { suit = Hearts; value = Six };
        { suit = Clubs; value = Ten };
        { suit = Clubs; value = Queen };
        { suit = Hearts; value = Queen };
      ];
    deck_rem =
      [
        Some { suit = Spades; value = Nine };
        Some { suit = Hearts; value = Jack };
        Some { suit = Spades; value = Six };
        Some { suit = Spades; value = King };
        Some { suit = Clubs; value = Six };
        Some { suit = Clubs; value = Two };
        Some { suit = Clubs; value = Four };
        Some { suit = Clubs; value = Nine };
        Some { suit = Diamonds; value = Ten };
        Some { suit = Diamonds; value = Three };
        Some { suit = Clubs; value = Five };
        Some { suit = Spades; value = Four };
        Some { suit = Diamonds; value = Four };
        Some { suit = Diamonds; value = Six };
        Some { suit = Hearts; value = Ten };
        Some { suit = Diamonds; value = Seven };
        Some { suit = Spades; value = Ten };
        Some { suit = Diamonds; value = Jack };
        Some { suit = Hearts; value = Five };
        Some { suit = Hearts; value = Nine };
        Some { suit = Diamonds; value = Five };
        Some { suit = Diamonds; value = Two };
        Some { suit = Diamonds; value = Ace };
        Some { suit = Hearts; value = Eight };
        Some { suit = Clubs; value = Seven };
        Some { suit = Hearts; value = Four };
        Some { suit = Diamonds; value = King };
        Some { suit = Hearts; value = Ace };
        Some { suit = Spades; value = Three };
        Some { suit = Clubs; value = King };
        Some { suit = Spades; value = Two };
        Some { suit = Spades; value = Eight };
        Some { suit = Clubs; value = Three };
        Some { suit = Diamonds; value = Queen };
        Some { suit = Hearts; value = King };
        Some { suit = Hearts; value = Seven };
        Some { suit = Spades; value = Seven };
        Some { suit = Diamonds; value = Nine };
        Some { suit = Spades; value = Queen };
        Some { suit = Spades; value = Ace };
        Some { suit = Spades; value = Five };
        Some { suit = Clubs; value = Ace };
        Some { suit = Clubs; value = Jack };
      ];
    turn = Player;
    user_money = 1000;
    cpu_moneys = [| 100; 0 |];
    dealer = Player;
    current_bet = 0;
  }

let t_state_2 =
  {
    users_hand =
      [
        { suit = Diamonds; value = Eight };
        { suit = Hearts; value = Three };
      ];
    cpu_hands =
      [|
        [
          { suit = Clubs; value = Eight };
          { suit = Spades; value = Jack };
        ];
      |];
    cards_on_table =
      [
        { suit = Hearts; value = Two };
        { suit = Hearts; value = Six };
        { suit = Clubs; value = Ten };
        { suit = Clubs; value = Queen };
        { suit = Hearts; value = Queen };
      ];
    deck_rem =
      [
        Some { suit = Spades; value = Nine };
        Some { suit = Hearts; value = Jack };
        Some { suit = Spades; value = Six };
        Some { suit = Spades; value = King };
        Some { suit = Clubs; value = Six };
        Some { suit = Clubs; value = Two };
        Some { suit = Clubs; value = Four };
        Some { suit = Clubs; value = Nine };
        Some { suit = Diamonds; value = Ten };
        Some { suit = Diamonds; value = Three };
        Some { suit = Clubs; value = Five };
        Some { suit = Spades; value = Four };
        Some { suit = Diamonds; value = Four };
        Some { suit = Diamonds; value = Six };
        Some { suit = Hearts; value = Ten };
        Some { suit = Diamonds; value = Seven };
        Some { suit = Spades; value = Ten };
        Some { suit = Diamonds; value = Jack };
        Some { suit = Hearts; value = Five };
        Some { suit = Hearts; value = Nine };
        Some { suit = Diamonds; value = Five };
        Some { suit = Diamonds; value = Two };
        Some { suit = Diamonds; value = Ace };
        Some { suit = Hearts; value = Eight };
        Some { suit = Clubs; value = Seven };
        Some { suit = Hearts; value = Four };
        Some { suit = Diamonds; value = King };
        Some { suit = Hearts; value = Ace };
        Some { suit = Spades; value = Three };
        Some { suit = Clubs; value = King };
        Some { suit = Spades; value = Two };
        Some { suit = Spades; value = Eight };
        Some { suit = Clubs; value = Three };
        Some { suit = Diamonds; value = Queen };
        Some { suit = Hearts; value = King };
        Some { suit = Hearts; value = Seven };
        Some { suit = Spades; value = Seven };
        Some { suit = Diamonds; value = Nine };
        Some { suit = Spades; value = Queen };
        Some { suit = Spades; value = Ace };
        Some { suit = Spades; value = Five };
        Some { suit = Clubs; value = Ace };
        Some { suit = Clubs; value = Jack };
      ];
    turn = Player;
    user_money = 0;
    cpu_moneys = [| 100; 100 |];
    dealer = Player;
    current_bet = 0;
  }

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

(* **** Compare helper functions **** *)
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

let find_best_hand
    (name : string)
    (state : State.state)
    (player : State.players)
    (expected : win_record list) : test =
  name >:: fun _ -> assert_equal expected (find_best_hand state player)

(* **** MAIN HELPER FUNCTIONS **** *)

(* [string_of_card_test] constructs an OUnit test named [name] that
   asserts the string produced from [card] is what is expected. *)
let string_of_card_test
    (name : string)
    (expected : string)
    (card : Deck.card) : test =
  name >:: fun _ -> assert_equal expected (string_of_card card)

(* [string_of_cards_test] constructs an OUnit test named [name] that
   asserts the string produced from the card list [cards] is what is
   expected. *)
let string_of_cards_test
    (name : string)
    (expected : string)
    (cards : Deck.card list) : test =
  name >:: fun _ -> assert_equal expected (string_of_cards cards)

let to_winner_test
    (name : string)
    (expected : int array)
    (winners : win_record list)
    (state : State.state)
    (adding : int array) : test =
  name >:: fun _ ->
  reset;
  for i = 0 to 7 do
    if i = 0 then add adding.(i) Player else add adding.(i) (Computer i)
  done;
  let to_win = to_winner winners state in
  for i = 0 to 7 do
    assert_equal expected.(i) to_win.(i)
  done

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
    (* hand_sort_int_test "Sort numbers" (hand_converter [] (total_hand
       t_player_hand t_table_river)) [ { string_suit = "♠"; int_value =
       2 }; { string_suit = "♠"; int_value = 3 }; { string_suit = "♠";
       int_value = 6 }; { string_suit = "♠"; int_value = 8 }; {
       string_suit = "♠"; int_value = 9 }; { string_suit = "♠";
       int_value = 12 }; { string_suit = "♠"; int_value = 13 }; ]; *)
    find_best_hand "Test player" t_state Player
      [ { player = Player; rank = 2; value = 12 } ];
  ]

let main_test =
  [
    string_of_card_test "Ace of Spades" "A♠"
      { suit = Spades; value = Ace };
    string_of_card_test "Queen of Hearts" "Q♥"
      { suit = Hearts; value = Queen };
    string_of_card_test "Two of Clubs" "2♣"
      { suit = Clubs; value = Two };
    string_of_card_test "Seven of Diamonds" "7♦"
      { suit = Diamonds; value = Seven };
    string_of_cards_test "Set of two cards" " A♠ Q♥"
      [
        { suit = Spades; value = Ace }; { suit = Hearts; value = Queen };
      ];
    string_of_cards_test "Set of four cards" " A♠ Q♥ 2♣ 7♦"
      [
        { suit = Spades; value = Ace };
        { suit = Hearts; value = Queen };
        { suit = Clubs; value = Two };
        { suit = Diamonds; value = Seven };
      ];
  ]

let pot_test =
  [
    to_winner_test "User wins, no side pot"
      [| 75; 0; 0; 0; 0; 0; 0; 0 |]
      [
        { player = Player; rank = 1; value = 1000 };
        { player = Computer 1; rank = 2; value = 500 };
        { player = Computer 2; rank = 3; value = 100 };
      ]
      t_state
      [| 25; 25; 25; 0; 0; 0; 0; 0 |];
    to_winner_test "Computer player wins, no side pot"
      [| 0; 75; 0; 0; 0; 0; 0; 0 |]
      [
        { player = Player; rank = 2; value = 500 };
        { player = Computer 1; rank = 1; value = 1000 };
        { player = Computer 2; rank = 3; value = 100 };
      ]
      t_state
      [| 25; 25; 25; 0; 0; 0; 0; 0 |];
    to_winner_test "Tie, no side pot"
      [| 45; 45; 0; 0; 0; 0; 0; 0 |]
      [
        { player = Player; rank = 1; value = 1000 };
        { player = Computer 1; rank = 1; value = 1000 };
        { player = Computer 2; rank = 3; value = 100 };
      ]
      t_state
      [| 30; 30; 30; 0; 0; 0; 0; 0 |];
    to_winner_test "User wins with computer caused side pot"
      [| 55; 0; 0; 0; 0; 0; 0; 0 |]
      [
        { player = Player; rank = 1; value = 1000 };
        { player = Computer 1; rank = 2; value = 500 };
        { player = Computer 2; rank = 3; value = 100 };
      ]
      t_state_1
      [| 25; 25; 5; 0; 0; 0; 0; 0 |];
    to_winner_test "User wins with their own caused side pot"
      [| 15; 40; 0; 0; 0; 0; 0; 0 |]
      [
        { player = Player; rank = 1; value = 1000 };
        { player = Computer 1; rank = 2; value = 500 };
        { player = Computer 2; rank = 3; value = 100 };
      ]
      t_state_2
      [| 25; 25; 5; 0; 0; 0; 0; 0 |];
  ]

let suite =
  "test suite for A2"
  >::: List.flatten
         [
           deck_test; table_test; compare_test; (*main_test;*) pot_test;
         ]

let _ = run_test_tt_main suite
