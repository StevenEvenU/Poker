open Deck 

(** The turn type decides whether it is the computer's or the player's
turn. *)
type players =
  | Player
  | Computer

(** The state type is keeping track of the cards that the user and player has 
along with keeping track was cards are on the table and how many cards are 
remaining in the deck. *)
type state = {
  users_hand: Deck.card list; 
  cpu_hand: Deck.card list; 
  cards_on_table: Deck.card list; 
  deck_rem: Deck.card array;
  turn: players;
}

(** This will delegate the cards amongst the players, thus changing the 
state. *)
val delegate: state -> state

(** This will deal the first three cards on the table. *)
val deal: state -> state

(** This will deal the next card on the table. *)
val flop: state -> state

(** This will check who won when the size of the state.cards_on_table is equal
to 5. *)
val round_check: state -> players
