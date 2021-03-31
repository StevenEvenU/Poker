type card_check

type win_record = {
  player : Table.players;
  rank : int;
  value : int;
}

val hand_of_rank : int -> string

val int_of_val : Deck.value -> int

val string_of_suit : Deck.suit -> string

val total_hand : Deck.card list -> Deck.card list -> Deck.card list

val hand_converter :
  card_check list -> Deck.card list -> card_check list

val hand_sort_int : card_check list -> card_check list

val best_hand : card_check list -> Table.players -> win_record

(** Returns win_record of the best hand a player has. *)
val find_best_hand : Table.state -> Table.players -> win_record