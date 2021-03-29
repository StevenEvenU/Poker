(* Player's action *)
type action = Check | Call | Raise | Fold

(* Checks whether someone has raised or not *)
val is_raised : Table.active_state -> bool

(* Gets the users next action *)
val get_action : Table.active_state -> action
