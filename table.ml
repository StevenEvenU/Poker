open Deck
(** Write in a function recursively that will keep on changing the init state based on how many players there are *)
type players = Player | Computer

type state = {
  users_hand: Deck.card list; 
  cpu_hand: Deck.card list; 
  cards_on_table: Deck.card list; 
  deck_rem: Deck.deck;
  turn: players;
}
let init_state = {users_hand = []; cpu_hand = []; cards_on_table = []; deck_rem = Deck.create (); turn = Player}

let delegate state = shuffle state.deck_rem;
  for i = 0 to 1 do
    {state with cpu_hand = (remove state.deck_rem :: state.cpu_hand); deck_rem = Deck.deck} 
  done;
  for i = 0 to 1 do
    {state with users_hand = (remove state.deck_rem) :: state.users_hand; deck_rem = Deck.deck}
  done

let deal state =
  for i = 0 to 2 do
    {state with cards_on_table = remove state.deck_rem:: state.cards_on_table; deck_rem = Deck.deck}
  done

let flop state = {state with cards_on_table = remove state.deck_rem :: state.cards_on_table; deck_rem = Deck.deck}

let winner state = failwith "Not implemented"

let round_check state = if List.length (state.cards_on_table) = 5 
  then winner state
  else flop state

  