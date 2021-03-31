open Deck
open Compare

(** Returns string of a players type  *)
val string_of_player : players -> string

(** The state type is keeping track of the cards that the user and
    player has along with keeping track was cards are on the table and
    how many cards are remaining in the deck. *)
type state = {
  mutable users_hand : Deck.card list;
  mutable cpu_hands : Deck.card list array;
  mutable cards_on_table : Deck.card list;
  mutable deck_rem : Deck.deck;
  mutable turn : players;
}

(** The active state of a game. It is inputted the number of computer
    players. *)
val active_state : int -> state

(** This will delegate the cards amongst the players, thus changing the
    state. *)
val delegate : state -> unit

(** This will deal the first three cards on the table. *)
val deal : state -> unit

(** This will deal the next card on the table. *)
val flop : state -> unit

(** Returns the winner of the round. *)
val winner : state -> Compare.win_record

(** This will check who won when the size of the state.cards_on_table is
    equal to 5. *)
val round_check : state -> unit
