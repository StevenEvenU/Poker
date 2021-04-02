(** Type card_check turns the card suit into a string and the card value
    into an int *)
type card_check

(** Type win_record holds knowledge of which players, the rank of their
    hand, and a value for tiebreakers *)
type win_record = {
  player : State.players;
  rank : int;
  value : int;
}

(** hand_of_rank takes the integer and returns a string with the
    correlating hand. *)
val hand_of_rank : int -> string

(** int_of_val takes the Deck.value of the deck and converts it to an
    int for easier in compare.ml. *)
val int_of_val : Deck.value -> int

(** string_of_suit takes the Deck.suit and returns a string with the
    unicode for that suit. *)
val string_of_suit : Deck.suit -> string

(** total_hand combines the two lists of Deck.card into one list *)
val total_hand : Deck.card list -> Deck.card list -> Deck.card list

(** converts Deck.card list into card_check list which has the suits in
    type string and the values in type int. *)
val hand_converter :
  card_check list -> Deck.card list -> card_check list

(** hand_sort_int takes the card_check list and sorts the hand by the
    cards integer values. *)
val hand_sort_int : card_check list -> card_check list

(** Given a player's available cards (and the player), this returns what
    their best available hand is. *)
val best_hand : card_check list -> State.players -> win_record

(** Returns win_record of the best hand a player has. *)
val find_best_hand : State.state -> State.players -> win_record list
