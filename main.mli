(** Player's action *)
type action = Check | Call | Raise | Fold

(** Checks whether someone has raised or not *)
val is_raised : State.state -> bool

(** Gets the users next action *)
val get_action : State.state -> action

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
