(** Brute force strategy of calculating aproximate probability of
    winning, given your two cards and whatever is on the table. Does
    this by randomly assigning other players remaining cards and
    completing the cards on the table and then finding the winner. Do
    the above 100 times and the number of wins divided by 100 is the
    aproximate probability.*)

open State
open Compare
open Deck

val prob : Deck.card list -> float
