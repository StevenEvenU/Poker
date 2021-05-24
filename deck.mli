(** The module Deck creates the deck along with performing other
    functionalities on the deck in order to play the game. *)

(** The type of suits that are used in a deck of cards. *)
type suit =
  | Spades
  | Hearts
  | Diamonds
  | Clubs

(** The type of values used in a deck of cards. *)
type value =
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace

(** The type of a single card. *)
type card = {
  suit : suit;
  value : value;
}

(** The type of cards in a deck.*)
type deck = card option list

(** [create_card] creates a card with the given value and suit*)
val create_card : value -> suit -> card

(** [create] is an unshuffled full deck of cards*)
val create : deck

(** [shuffle deck] is [deck] randomly shuffled*)
val shuffle : deck -> deck

(** [top_card deck] is the top card from [deck]*)
val top_card : deck -> card option

(** [remove_top deck] is [deck] with the top card removed*)
val remove_top : deck -> deck

(** [size deck] is the number of cards in [deck]*)
val size : deck -> int
