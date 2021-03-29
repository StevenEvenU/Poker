(* Player's action *)
type action = Check | Call | Raise | Fold

(* Checks whether someone has raised or not *)
val is_raised : Table.state -> bool

(* Gets the users next action *)
val get_action : Table.state -> action

(* Returns string of suit *)
val string_of_suit : Deck.suit -> string

(* Returns string of value *)
val string_of_value : Deck.value -> string

(* Returns string of card *)
val string_of_card : Deck.card -> string

(* Returns string of card list *)
val string_of_cards : string -> Deck.card list -> string