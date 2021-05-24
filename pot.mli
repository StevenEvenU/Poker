(** The module Pot stores the pot and consists of functions necessary to
    the game of Poker *)
open State

open Compare

(** [add] sees if money should be added to the pot and does so if
    needed. Must be given the player betting and the amount (IMPORTANT:
    give 0 if a player is calling, give a negative number if a player is
    folding). *)
val add : int -> State.players -> unit

(** [reset] sets pot to zero and prepares all mutable variables for
    future rounds*)
val reset : unit -> unit

(** [print_pot] is the total sum of everything in the pot*)
val print_pot : unit -> string

(** [top_winners] is the list of winners' (ties included) from the end
    of a round integer locations in an array (the user is 0,
    [Computer x] is x) *)
val top_winners : win_record list -> int list

(** [to_winner] splits the pot amongs the winners, using a side pot if
    necessary.*)
val to_winner : win_record list -> State.state -> int array
