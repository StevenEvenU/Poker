(** Player's action *)
type action =
  | Check
  | Call
  | Raise
  | Fold

(** Returns string of value *)
val string_of_value : Deck.value -> string

(** Returns string of card *)
val string_of_card : Deck.card -> string

(** Returns string of card list *)
val string_of_cards : Deck.card list -> string

(**  *)
val print_hand : Deck.card list -> State.players -> unit

(** Returns string of the state *)
val print_event : State.state -> string -> unit

(** Updates state.turn and state.current_bet *)
val next_turn : State.state -> State.players array ref -> int -> unit

(** Updates the bets array that keeps track of each persons bet in a
    round of betting *)
val update_bets :
  int array -> State.players -> State.state -> int -> unit

(** Gets the players previous bet in this round *)
val player_prev_bet : State.state -> int array -> int

(** Retrieves this players money from state *)
val get_money : State.state -> State.players -> int

(** Checks if this person can check or not *)
val valid_check : State.state -> int array -> bool

(** Checks if this person can call or not *)
val valid_call : State.state -> int array -> bool

(** Checks if this person can raise or not *)
val valid_raise : State.state -> int array -> bool

(** Asks the user how much they want to raise by *)
val get_raise_amount : State.state -> int

(** Prompts the user what they wish to do: Check, Call, Raise, or Fold
    and then does so*)
val prompt_action :
  State.state -> State.players array ref -> int array -> int

(** Iterates through each player at the table during betting round *)
val rec_betting_round :
  State.state ->
  State.players array ref ->
  int array ->
  State.players ->
  int ->
  int

(** Main function to start round of betting *)
val betting_round : State.state -> State.players array ref -> unit
