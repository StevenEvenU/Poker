(** The deck of cards in order. Able to be shuffled.*)

(** The type of suits. *)
type suit =
  | Spades
  | Hearts
  | Diamonds
  | Clubs

(** The type of values. *)
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

(** The abstract type of cards in order in a deck.*)
type deck

(** Creates a card value of suit*)
val create_card : value -> suit -> card

(** Creates a shuffled deck of cards and returns it*)
val create : deck

(** Shuffles a deck (or smaller amount) of cards*)
val shuffle : deck -> deck

(** Gives the top card from the deck*)
val top_card : deck -> card option

(** Removes the top card from the deck*)
val remove_top : deck -> deck

(** Returns size of deck*)
val size : deck -> int
