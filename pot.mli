(** The money on the table*)
open State

open Compare

(** Add money to the pot. Must be given the state, the amount
    (IMPORTANT: give 0 if a player is calling, give -1 if a player is
    folding), and the player betting *)
val add : int -> State.players -> unit

(** Set pot to zero*)
val reset : unit -> unit

(** Splits the pot amongs the winners. Uses a side pot if necessary.
    Assumes it is passed just the winners (one winner, ties, etc.).*)
val to_winner : win_record list -> State.state -> int array
