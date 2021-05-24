(** The module State helps keep track of values changing throughout the
    game such as users hand, hands of computers, cards on table, the
    remaining deck, whose turn it is, users money, the money that the
    cpu has, the dealer, and the current bet. *)

(** A type to identify a player and different computers. *)
type players =
  | Player
  | Computer of int

(** Returns integer of a players type *)
val int_of_player : players -> int

(** Returns player type of a corresponding integer *)
val player_of_int : int -> players

(** Returns the string of a players type *)
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
