open Deck
open Compare

let string_of_player = function
  | Player -> "Player"
  | Computer -> "Opponent"

type state = {
  mutable users_hand : Deck.card list;
  mutable cpu_hands : Deck.card list array;
  mutable cards_on_table : Deck.card list;
  mutable deck_rem : Deck.deck;
  mutable turn : Compare.players;
}

let table_deck = create

let init_state (num : int) =
  {
    users_hand = [];
    cpu_hands = Array.make num [];
    cards_on_table = [];
    deck_rem = table_deck;
    turn = Player;
  }

let active_state (num : int) = init_state num

let delegate state =
  Random.self_init ();
  state.deck_rem <- shuffle state.deck_rem (Random.int 100000);
  for j = 0 to Array.length state.cpu_hands - 1 do
    for i = 0 to 1 do
      state.cpu_hands.(j) <-
        (match top_card state.deck_rem with
        | Some card -> card :: state.cpu_hands.(j)
        | none -> state.cpu_hands.(j));
      state.deck_rem <- remove_top state.deck_rem
    done
  done;
  for i = 0 to 1 do
    state.users_hand <-
      (match top_card state.deck_rem with
      | Some card -> card :: state.users_hand
      | none -> state.users_hand);
    state.deck_rem <- remove_top state.deck_rem
  done

let deal state =
  for i = 0 to 2 do
    state.cards_on_table <-
      (match top_card state.deck_rem with
      | Some card -> card :: state.cards_on_table
      | none -> state.cards_on_table);
    state.deck_rem <- remove_top state.deck_rem
  done

let flop state =
  state.cards_on_table <-
    (match top_card state.deck_rem with
    | Some card -> card :: state.cards_on_table
    | none -> state.cards_on_table);
  state.deck_rem <- remove_top state.deck_rem

let winner (state : state) =
  let best_player = find_best_hand state Player in
  let best_computers = find_best_hand Computer in
  let hands = List.append best_player best_computers in
  hands  
  
let round_check state =
  if List.length state.cards_on_table = 5 then winner state
  else flop state