(** The money on the table*)
open Compare

(** The money on the table (the pot)*)
type pot

(** Add money to the pot*)
val add : int -> unit

(** Splits the pot amongs the winners. Uses a side pot if necessary.*)
val to_winner : win_record list -> int list
