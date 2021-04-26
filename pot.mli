(** The money on the table*)
open Compare

(** The money on the table (the pot)*)
type pot

(** Add money to the pot*)
val add : int -> bool -> unit

(** Set pot to zero*)
val reset : unit -> unit

(** Splits the pot amongs the winners. Uses a side pot if necessary.
    Assumes it is passed just the winners (one winner, ties, etc.).*)
val to_winner : win_record list -> int list
