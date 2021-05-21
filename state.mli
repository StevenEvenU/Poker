(** The turn type decides whether it is the computer's or the player's
    turn. *)
type players =
  | Player
  | Computer of int

(** Returns integer of a players type *)
val int_of_player : players -> int

(** Returns player type of corresponding integer *)
val player_of_int : int -> players

(** Returns string of a players type *)
val str_of_player : players -> string

(** The state type is keeping track of the cards that the user and
    player has along with keeping track was cards are on the table and
    how many cards are remaining in the deck. *)
type state = {
  mutable users_hand : Deck.card list;
  mutable cpu_hands : Deck.card list array;
  mutable cards_on_table : Deck.card list;
  mutable deck_rem : Deck.deck;
  mutable turn : players;
  mutable user_money : int;
  mutable cpu_moneys : int array;
  mutable dealer : players;
  mutable current_bet : int;
}

(** bankrupt checks if the person is going all in. If so it will return
    true, otherwise it will return false *)
val bankrupt : int -> state -> bool
