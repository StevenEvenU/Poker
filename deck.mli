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

(** The abstract type of cards in order in a deck.*)
type deck = card option list

(* TODO: DELETE THIS. TEMPORARY ONLY *)
val to_deck : card option list -> deck

(** Creates a card value of suit*)
val create_card : value -> suit -> card

(** Creates a shuffled deck of cards and returns it*)
val create : deck

(* TODO: DELETE THIS. TEMPORARY ONLY val shuffle_helper : deck -> card
   option list -> int -> int -> card option list val shuffle_repeater :
   deck -> card option list -> int -> int -> int -> deck *)

(** Shuffles a deck of cards*)
val shuffle : deck -> deck

(** Gives the top card from the deck*)
val top_card : deck -> card option

(** Removes the top card from the deck*)
val remove_top : deck -> deck

(** Returns size of deck*)
val size : deck -> int
