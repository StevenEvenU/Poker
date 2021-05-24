(** The module Probability is a brute force strategy. It calculates the
    aproximate probability of winning, given your two cards and whatever
    is on the table. It does this by randomly assigning other players
    remaining cards and completing the cards on the table and then
    finding the winner. Do the above [repetitions] number of times and
    the number of wins divided by [reptitions] is the aproximate
    probability.*)

open State
open Compare
open Deck

(** [prob] is the approximate probability of winning the hand given a
    player's cards and cards on the table and the number of players.
    [prob] is given a list of cards (with the FIRST TWO the cards in the
    hand and the rest the ones on the table) as well as the number of
    players *)
val prob : Deck.card list -> int -> float

val print_prob : Deck.card list -> int -> unit
