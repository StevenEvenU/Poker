(** The module Mainfuncs is a module consisting of the functions that
    are constantly used in main. Consisting of essentially making types
    in deck into strings for the terminal to use. *)

(** Returns string of value. *)
val str_of_val : Deck.value -> string

(** Returns string of card. *)
val str_of_card : Deck.card -> string

(** Returns string of card list. *)
val str_of_cards : Deck.card list -> string
