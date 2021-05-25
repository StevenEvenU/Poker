(** TEST PLAN.

    Modules were tested as they were being created. Authors mostly made
    the tests for the modules which they worked on, but occasionally we
    worked on each others tests.

    Deck Module Testing: This module was primarily tested by the OUnit
    test cases. I used a mixture of black box and white box testing to
    test each of the functions in [deck.mli]. The white box testing was
    used for [create], [top_card], and [remove_card], since those tests
    depended on the order of the unshuffled deck, which is determined in
    the code itself. For [size] and [create_card], regular black box
    testing was used. I knew what I needed to input and what I wanted to
    get from the comment in the mli file. The function [shuffle] also
    used black box testing, but because it depends on random numbers, I
    could never knew what result I would get. The way I tested it was by
    seeing that no two shuffled decks were the same, though of course
    there is a very small chance of this test failing even if the code
    is correct due to its probabilastic nature.

    Pot Module Testing: This module was primarily tested by the OUnit
    test cases, but occasionally errors were found while playing the
    game and then corrected. Tests were done using mostly black box
    testing but with some white box testing (I knew some situations
    (such as players going all in) would activate helper functions, so I
    made test cases to go through those functions). Rather than testing
    the functions in the modeule independently, the tests were done by
    using the Pot Module as it would be used during a game. First
    [reset] was called, then inputs (bet amounts or folds) were added
    iteratively using [add] and then [to_winners] was called, which used
    [top_winners]. Note that [print_pot] was not explicitely tested, but
    it is simply an [string_of_int] of [piling], which is an extensively
    used helper function of [to_winners], and so is guaranteed to work
    if [to_winners] works. *)

open OUnit2
open Deck
open Compare
open Main
open Table
open State
open Pot
open Betting
open Probability

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

let t_players_in = ref [| Player |]
let t_players_in_1 = ref [| Computer 1 |]
let t_players_in_2 = ref [| Player; Computer 1 |]
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

let t_state_alt =
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
    turn = Computer 1;
    user_money = 1000;
    cpu_moneys = Array.make 2 1000;
    dealer = Player;
    current_bet = 0;
  }
  
(** The following states are only for testing pot*)
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
        [
          { suit = Clubs; value = Four }; { suit = Clubs; value = Nine };
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
        [
          { suit = Hearts; value = Five };
          { suit = Hearts; value = Nine };
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
    current_bet = 50;
  }

let t_state_3 =
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
        [
          { suit = Diamonds; value = Ten };
          { suit = Diamonds; value = Three };
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
    turn = Computer 2;
    user_money = 110;
    cpu_moneys = [| 0; 0 |];
    dealer = Player;
    current_bet = 0;
  }

let t_state_3_alt =
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
    turn = Computer 2;
    user_money = 110;
    cpu_moneys = [| 0; 0 |];
    dealer = Player;
    current_bet = 0;
  }

let t_state_4 =
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
        [
          { suit = Spades; value = Five }; { suit = Clubs; value = Ace };
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
        Some { suit = Clubs; value = Jack };
      ];
    turn = Computer 2;
    user_money = 0;
    cpu_moneys = [| 0; 800 |];
    dealer = Player;
    current_bet = 0;
  }

let t_state_5 =
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
        [
          { suit = Spades; value = Queen };
          { suit = Diamonds; value = Ten };
        ];
        [
          { suit = Clubs; value = King }; { suit = Spades; value = Two };
        ];
        [
          { suit = Diamonds; value = Three };
          { suit = Clubs; value = Five };
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
        Some { suit = Diamonds; value = Ace };
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
        Some { suit = Spades; value = Ace };
        Some { suit = Hearts; value = Eight };
        Some { suit = Clubs; value = Seven };
        Some { suit = Hearts; value = Four };
        Some { suit = Diamonds; value = King };
        Some { suit = Hearts; value = Ace };
        Some { suit = Spades; value = Three };
        Some { suit = Spades; value = Eight };
        Some { suit = Clubs; value = Three };
        Some { suit = Diamonds; value = Queen };
        Some { suit = Hearts; value = King };
        Some { suit = Hearts; value = Seven };
        Some { suit = Spades; value = Seven };
        Some { suit = Diamonds; value = Nine };
        Some { suit = Spades; value = Five };
        Some { suit = Clubs; value = Ace };
        Some { suit = Clubs; value = Jack };
      ];
    turn = Computer 2;
    user_money = 0;
    cpu_moneys = [| 0; 0; 0; 0 |];
    dealer = Player;
    current_bet = 0;
  }

(********START HELPER FUNCTIONS*************)

let arr_to_str str_start arr =
  try
    str_start := "[|" ^ string_of_int arr.(0);
    for i = 1 to 7 do
      str_start := !str_start ^ "," ^ string_of_int arr.(i)
    done;
    str_start := !str_start ^ "|]";
    !str_start
  with _ -> string_of_int (Array.length arr)

(** converts card option to string*)
let string_of_card_option card1 =
  match card1 with None -> "" | Some x -> str_of_card x

(** converts card option list to string*)
let rec string_of_card_options str cards =
  match cards with
  | [] -> str
  | [ h ] -> (
      match h with None -> str | Some x -> str ^ " " ^ str_of_card x)
  | h :: t -> (
      match h with
      | None ->
          let s = str in
          string_of_card_options s t
      | Some x ->
          let s = str ^ " " ^ str_of_card x in
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
  assert_equal expected (str_of_card (create_card valu sut))

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
  name >:: fun _ -> assert_equal expected (str_of_suit suit)

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

(* **** TABLE HELPER FUNCTIONS **** *)
let bet_test
    (name : string)
    (player : State.players)
    (amt : int)
    (state : state)
    (expected : int) : test =
  name >:: fun _ ->
  let old_pot = print_pot () in
  bet player amt state;
  assert_equal expected
    (int_of_string (print_pot ()) - int_of_string old_pot)

let distr_test
    (name : string)
    (int_arr : int array)
    (state : State.state)
    (player_count : int)
    (expected : int) : test =
  name >:: fun _ ->
  distr int_arr state player_count;
  assert_equal expected state.user_money

(* **** MAIN HELPER FUNCTIONS **** *)

(* [string_of_card_test] constructs an OUnit test named [name] that
   asserts the string produced from [card] is what is expected. *)
let string_of_card_test
    (name : string)
    (expected : string)
    (card : Deck.card) : test =
  name >:: fun _ -> assert_equal expected (str_of_card card)

(* [string_of_cards_test] constructs an OUnit test named [name] that
   asserts the string produced from the card list [cards] is what is
   expected. *)
let string_of_cards_test
    (name : string)
    (expected : string)
    (cards : Deck.card list) : test =
  name >:: fun _ -> assert_equal expected (str_of_cards cards)

(** [pot_all_test] constructs an OUnit test named [name] that asserts
    with [expected] how the pot is constructed given players
    contributing money corresponding to [adding] to the pot and then
    split amongs the properly according to [winners]. This tests
    [reset], [add], [to_winner], and [top_winner]*)
let pot_all_test
    (name : string)
    (expected : int array)
    (winners : win_record list)
    (state : State.state)
    (adding : int array) : test =
  name >:: fun _ ->
  Pot.reset ();
  for i = 0 to 7 do
    if i = 0 then add adding.(i) Player else add adding.(i) (Computer i)
  done;
  let to_win = to_winner winners state in
  assert_equal expected to_win ~printer:(arr_to_str (ref ""))


(* **** BETTING HELPER FUNCTIONS **** *)

(** [next_turn_test] constructs an OUnit test named [name] that asserts
    with [expected] and [bet_num] how the state changes after
    [next_turn] with the [state] and [play_arr]. *)
let next_turn_test
    (name : string)
    (expected : players)
    (state : State.state)
    (play_arr : players array ref)
    (bet_num : int) : test =
  name >:: fun _ ->
  next_turn state play_arr bet_num;
  assert_equal expected state.turn;
  assert_equal state.current_bet bet_num

(** [get_money_test] constructs an OUnit test named [name] that asserts
    with [expected] and [get_money state play]. *)
let get_money_test
    (name : string)
    (expected : int)
    (state : State.state)
    (play : players) : test =
  name >:: fun _ -> assert_equal expected (get_money state play)

(** [valid_check_test] constructs an OUnit test named [name] that
    asserts with [expected] and [valid_check state state.turn arr]. *)
let valid_check_test
    (name : string)
    (expected : bool)
    (state : State.state)
    (arr : int array) : test =
  name >:: fun _ ->
  assert_equal expected (valid_check state state.turn arr)

(** [valid_call_test] constructs an OUnit test named [name] that asserts
    with [expected] and [valid_call state state.turn play]. *)
let valid_call_test
    (name : string)
    (expected : bool)
    (state : State.state)
    (arr : int array) : test =
  name >:: fun _ ->
  assert_equal expected (valid_call state state.turn arr)

(** [prob_test] constructs an OUnit test named [name] that asserts that
    [prob lst n] is within an acceptable range of [expected]. IMPORTANT
    NOTE: this test may fail even if the code is correct due to its
    probabilistic nature. *)
let prob_test (name : string) (expected : float) (lst : Deck.card list)
    : test =
  name >:: fun _ ->
  assert_equal true (Float.abs (expected -. prob lst 3) < 0.15)

(* [player_index_test] constructs an OUnit test named [name] that
   asserts that [player_index player players_in 0] is equal to
   [expected]. *)
let player_index_test
    (name : string)
    (expected : int)
    (player : State.players)
    (players_in : State.players array ref) : test =
  name >:: fun _ ->
  assert_equal expected (player_index player players_in 0)

(* [iterate_player_test] constructs an OUnit test named [name] that
   asserts that [iterate_player idx players_in num] is equal to
   [expected] *)
let iterate_player_test
    (name : string)
    (expected : State.players)
    (idx : int)
    (players_in : State.players array ref)
    (num : int) : test =
  name >:: fun _ ->
  assert_equal expected (iterate_player idx players_in num)

(* [update_bets_test] constructs an OUnit test named [name] that asserts
   that [update_bets bets player state bet] is equal to [expected] *)

(* [player_prev_bet_test] constructs an OUnit test named [name] that asserts
   that [player_prev_bet player bets] is equal to [expected] *)
let player_prev_bet_test (name : string) (expected : int) (player : State.players) (bets : int array) : test = 
  name >:: fun _ -> assert_equal expected (player_prev_bet player bets)

(* [get_hand_test] constructs an OUnit test named [name] that asserts
   that [get_hand_bet state player] is equal to [expected] *)
let get_hand_test (name : string) (expected : Deck.card list) (state : State.state) (player : State.players) : test =
  name >:: fun _ -> assert_equal expected (get_hand state player)


(* *******END HELPER FUNCTIONS********* *)
let deck_test =
  [
    full_deck_test "Create unshuffled deck" create;
    full_deck_test "Create a shuffled deck" (shuffle create);
    decks_equality_test "See if two shuffled decks are different"
      (shuffle create) (shuffle create) false;
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

let table_test =
  [
    bet_test "Player add" Player 100 t_state 100;
    bet_test "Computer add" (Computer 1) 100 t_state 100;
    distr_test "Adding to player" [| 600; 100 |] t_state 1 1600;
  ]

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
      ]
  ]
let betting_test = 
  [
    get_money_test "Get player money" 1000 t_state_alt Player;
    get_money_test "Get computer 1 money" 100 t_state_1 (Computer 1);
    get_money_test "Get player money state_2" 0 t_state_2 Player;
    valid_check_test "First in round" true t_state_alt [| 0; 0; 0 |];
    valid_check_test "End round" true t_state_3_alt [| 4; 4; 4 |];
    valid_call_test "Call with enough money" true t_state_1
      [| 9; 9; 50 |];
    valid_call_test "Call with not enough money (bankrupt)" false
      t_state_2 [| 9; 9; 50 |];
    valid_call_test "Call with not enough money (too high)" true
      t_state_1 [| 9; 9; 50000 |];
    player_index_test "Player index t_players_in" 0 Player t_players_in;
    player_index_test "Player index t_players_in_2" 1 (Computer 1) t_players_in_2;
    iterate_player_test "Iterate 1 player" (Computer 1) 0 t_players_in_2 1;
    iterate_player_test "Iterate 0 player" Player 0 t_players_in_2 0;
    next_turn_test "Next turn after player, no one folded" (Computer 2)
      t_state_alt
      (ref [| Player; Computer 1; Computer 2 |])
      50;
    next_turn_test "Next turn after player, Computer 1 folded"
      (Computer 2) t_state
      (ref [| Player; Computer 2 |])
      50;
    next_turn_test
      "Next turn after last computer player, another computer folded \
       folded"
      Player t_state_3
      (ref [| Player; Computer 2 |])
      50;
    valid_check_test "Middle of round" false t_state_3 [| 5; 5; 0 |];

  ]

let pot_test =
  [
    pot_all_test "User wins, no side pot"
      [| 75; 0; 0; 0; 0; 0; 0; 0 |]
      [
        { player = Player; rank = 10; value = 1000 };
        { player = Computer 1; rank = 9; value = 500 };
        { player = Computer 2; rank = 8; value = 100 };
      ]
      t_state
      [| 25; 25; 25; 0; 0; 0; 0; 0 |];
    pot_all_test "Computer player wins, no side pot"
      [| 0; 75; 0; 0; 0; 0; 0; 0 |]
      [
        { player = Player; rank = 8; value = 500 };
        { player = Computer 1; rank = 10; value = 1000 };
        { player = Computer 2; rank = 7; value = 100 };
      ]
      t_state
      [| 25; 25; 25; 0; 0; 0; 0; 0 |];
    pot_all_test "Tie, no side pot"
      [| 45; 45; 0; 0; 0; 0; 0; 0 |]
      [
        { player = Player; rank = 10; value = 1000 };
        { player = Computer 1; rank = 10; value = 1000 };
        { player = Computer 2; rank = 3; value = 100 };
      ]
      t_state
      [| 30; 30; 30; 0; 0; 0; 0; 0 |];
    pot_all_test "User wins with computer caused side pot"
      [| 55; 0; 0; 0; 0; 0; 0; 0 |]
      [
        { player = Player; rank = 10; value = 1000 };
        { player = Computer 1; rank = 8; value = 500 };
        { player = Computer 2; rank = 7; value = 100 };
      ]
      t_state_1
      [| 25; 25; 5; 0; 0; 0; 0; 0 |];
    pot_all_test "User wins with their own caused side pot"
      [| 15; 40; 0; 0; 0; 0; 0; 0 |]
      [
        { player = Player; rank = 10; value = 1000 };
        { player = Computer 1; rank = 9; value = 500 };
        { player = Computer 2; rank = 3; value = 100 };
      ]
      t_state_2
      [| 5; 25; 25; 0; 0; 0; 0; 0 |];
    pot_all_test "User wins with two computer caused side pots"
      [| 35; 0; 0; 0; 0; 0; 0; 0 |]
      [
        { player = Player; rank = 10; value = 1000 };
        { player = Computer 1; rank = 6; value = 500 };
        { player = Computer 2; rank = 3; value = 100 };
      ]
      t_state_3
      [| 25; 5; 5; 0; 0; 0; 0; 0 |];
    pot_all_test
      "User wins with their own side pot and computer caused side pot \
       of different values"
      [| 30; 10; 35; 0; 0; 0; 0; 0 |]
      [
        { player = Player; rank = 10; value = 1000 };
        { player = Computer 1; rank = 7; value = 500 };
        { player = Computer 2; rank = 3; value = 100 };
      ]
      t_state_4
      [| 10; 15; 50; 0; 0; 0; 0; 0 |];
    pot_all_test
      "User loses with computer caused side pot of different values \
       test 2"
      [| 0; 10; 35; 0; 0; 0; 0; 0 |]
      [
        { player = Player; rank = 1; value = 10 };
        { player = Computer 1; rank = 7; value = 500 };
        { player = Computer 2; rank = 3; value = 100 };
      ]
      t_state_4
      [| 0; 5; 40; 0; 0; 0; 0; 0 |];
    pot_all_test "More players, everyone all in with different values"
      [| 0; 50; 139; 0; 0; 0; 0; 0 |]
      [
        { player = Player; rank = 1; value = 10 };
        { player = Computer 1; rank = 10; value = 500 };
        { player = Computer 2; rank = 7; value = 100 };
        { player = Computer 3; rank = 3; value = 100 };
        { player = Computer 4; rank = 2; value = 100 };
      ]
      t_state_5
      [| 46; 10; 80; 23; 30; 0; 0; 0 |];
  ]

(** IMPORTANT NOTE: there is a not insigificant probability of this
    failing even if the code is correct because it is based on
    probability using randomization. I selected ranges of possible
    values that are a valid enough interval to usually work, but there
    is always a chance that the value will fall outside the range*)
let probability_test =
  [
    prob_test "moderately low probability of hand winning pre-turn" 0.20
      [
        { suit = Hearts; value = Two };
        { suit = Hearts; value = Six };
        { suit = Clubs; value = Ten };
        { suit = Clubs; value = Queen };
        { suit = Hearts; value = Queen };
      ];
    prob_test "moderately low probability of hand winning pre-river"
      0.15
      [
        { suit = Hearts; value = Two };
        { suit = Hearts; value = Six };
        { suit = Clubs; value = Ten };
        { suit = Clubs; value = Queen };
        { suit = Hearts; value = Queen };
        { suit = Diamonds; value = Ace };
      ];
    prob_test "moderately high probability of hand winning pre-turn"
      0.85
      [
        { suit = Spades; value = Queen };
        { suit = Clubs; value = Nine };
        { suit = Clubs; value = Ten };
        { suit = Clubs; value = Queen };
        { suit = Hearts; value = Queen };
      ];
    prob_test "almost certain probability of hand winning pre-turn" 0.99
      [
        { suit = Spades; value = Queen };
        { suit = Diamonds; value = Queen };
        { suit = Clubs; value = Ten };
        { suit = Clubs; value = Queen };
        { suit = Hearts; value = Queen };
      ];
    prob_test "almost certain probability of hand losing pre-turn" 0.10
      [
        { suit = Diamonds; value = Two };
        { suit = Hearts; value = Three };
        { suit = Spades; value = Queen };
        { suit = Clubs; value = Jack };
        { suit = Clubs; value = King };
      ];
    prob_test "high probability of hand winning pre-river" 0.90
      [
        { suit = Hearts; value = Ace };
        { suit = Clubs; value = Ace };
        { suit = Clubs; value = Ten };
        { suit = Clubs; value = Queen };
        { suit = Hearts; value = Queen };
        { suit = Diamonds; value = Ace };
      ];
    prob_test "low probability of hand winning pre-river" 0.20
      [
        { suit = Hearts; value = Two };
        { suit = Clubs; value = Seven };
        { suit = Diamonds; value = Ten };
        { suit = Clubs; value = Queen };
        { suit = Clubs; value = Queen };
        { suit = Diamonds; value = Ace };
      ];
    prob_test "pocket aces pre-flop hand" 0.86
      [ { suit = Hearts; value = Ace }; { suit = Clubs; value = Ace } ];
  ]

let suite =
  "test suite for A2"
  >::: List.flatten
         [
           deck_test;
           table_test;
           compare_test;
           main_test;
           pot_test;
           probability_test;
           betting_test;
         ]

let _ = run_test_tt_main suite
