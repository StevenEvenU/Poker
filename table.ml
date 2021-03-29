open Deck
(** Write in a function recursively that will keep on changing the init state based on how many players there are *)
type players = Player | Computer

type state = {
  mutable users_hand: Deck.card list; 
  mutable cpu_hand: Deck.card list; 
  mutable cards_on_table: Deck.card list; 
  mutable deck_rem: Deck.deck;
  mutable turn: players;
}

let table_deck = create()
let init_state = {users_hand = []; cpu_hand = []; cards_on_table = []; deck_rem = table_deck; turn = Player}

let delegate state = shuffle state.deck_rem;
  for i = 0 to 1 do
    state.cpu_hand <- (remove state.deck_rem :: state.cpu_hand);
    state.deck_rem <- table_deck
  done;
  for i = 0 to 1 do
    state.users_hand <- (remove state.deck_rem :: state.users_hand); 
    state.deck_rem <- table_deck
  done

let deal state =
  for i = 0 to 2 do
    state.cards_on_table <- (remove state.deck_rem:: state.cards_on_table);
    state.deck_rem <- table_deck
  done

let flop state = 
  state.cards_on_table <- (remove state.deck_rem :: state.cards_on_table);
  state.deck_rem <- table_deck

let winner state = failwith "Not implemented"

let round_check state = if List.length (state.cards_on_table) = 5 
  then winner state
  else flop state

  