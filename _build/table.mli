open Deck

(** The turn type decides whether it is the computer's or the player's
    turn. *)
type players =
  | Player
  | Computer

(** The state type is keeping track of the cards that the user and
    player has along with keeping track was cards are on the table and
    how many cards are remaining in the deck. *)
type state = {
  mutable users_hand : Deck.card list;
  mutable cpu_hand : Deck.card list;
  mutable cards_on_table : Deck.card list;
  mutable deck_rem : Deck.deck;
  mutable turn : players;
}

(** This will delegate the cards amongst the players, thus changing the
    state. *)
val delegate : state -> unit

(** This will deal the first three cards on the table. *)
val deal : state -> unit

(** This will deal the next card on the table. *)
val flop : state -> unit

(** This will check who won when the size of the state.cards_on_table is
    equal to 5. *)
val round_check : state -> unit
