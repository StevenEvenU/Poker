(** The module Main is where the interactivity between the player and
    the terminal takes place. *)

(** Player's action *)
type action =
  | Check
  | Call
  | Raise
  | Fold

(** Returns string of value. *)
val str_of_val : Deck.value -> string

(** Returns string of card. *)
val str_of_card : Deck.card -> string

(** Returns string of card list. *)
val str_of_cards : Deck.card list -> string

(** Returns the hand of a given player. *)
val print_hand : Deck.card list -> State.players -> unit

(** Returns string of the state. *)
val print_event : State.state -> string -> unit

(** Main function to start round of betting. *)
val betting_round : State.state -> State.players array ref -> unit
