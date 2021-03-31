open Deck

(** The active state of a game. It is inputted the number of computer
    players. *)
val active_state : int -> State.state

(** This will delegate the cards amongst the players, thus changing the
    state. *)
val delegate : State.state -> unit

(** This will deal the first three cards on the table. *)
val deal : State.state -> unit

(** This will deal the next card on the table. *)
val flop : State.state -> unit

(** Returns the winner of the round. *)
val winner : State.state -> Compare.win_record

(** This will check who won when the size of the state.cards_on_table is
    equal to 5. *)
(* val round_check : State.state -> unit *)
