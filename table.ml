open Deck
"Write in a function recursively that will keep on changing the init state based on how many players there are"

let INIT_STATE = {
  users_hand = []; 
  cpu_hand = []; 
  cards_on_table = []; 
  deck_rem = create; 
  turn: Player; 
}

let card_removed = remove

let delegate state = shuffle;
  for i = 0 to 1 do
    {state with cpu_hand = remove :: cpu_hand; deck_rem = deck}
  done
  for i = 0 to 1 do
    {state with users_hand = remove :: users_hand; deck_rem = deck}
  done

let deal state =
  for i = 0 to 2 do
    {state with cards_on_table = remove () :: cards_on_table; deck_rem = deck}
  done

let flop state = 
  {state with cards_on_table = remove () :: cards_on_table; deck_rem = deck}

let round_check state = if List.length (state.cards_on_table) = 5 
  then winner state
  else flop state

  