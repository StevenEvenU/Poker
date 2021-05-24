(** Updates state.turn and state.current_bet *)
val next_turn : State.state -> State.players array ref -> int -> unit

(** Returns index of player in the players_in array *)
val player_index : State.players -> State.players array ref -> int -> int

(** Updates the bets array that keeps track of each persons bet in a
    round of betting *)
val update_bets :
  int array -> State.players -> State.state -> int -> unit

(** Gets the players previous bet in this round *)
val player_prev_bet : State.players -> int array -> int

(** Retrieves this players money from state *)
val get_money : State.state -> State.players -> int

(** Asks the user how much they want to raise by *)
val get_raise_amt : State.state -> int

(** Checks if this person can check or not *)
val valid_check : State.state -> State.players -> int array -> bool

(** Checks if this person can call or not *)
val valid_call : State.state -> State.players -> int array -> bool

(** Checks if this person can raise or not *)
val valid_raise : State.state -> State.players -> int array -> bool

(** Prompts the user what they wish to do: Check, Call, Raise, or Fold
    and then does so*)
val prompt_action :
  State.state -> State.players array ref -> int array -> int

(** Iterates through each player at the table during betting round *)
val rec_bet_round :
  State.state -> State.players array ref -> int array -> int -> int

(* Last part of a betting round, where we make sure everyone has either
   called or folded *)
val last_call :
  State.state -> State.players array ref -> int array -> unit
