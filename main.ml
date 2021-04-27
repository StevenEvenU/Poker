open Deck
open Table
open Compare
open State

(** Types of user action *)
type action =
  | Check
  | Call
  | Raise
  | Fold

(** Checks whether another player has raised *)
let is_raised state = failwith "Not implemented yet"

(** Get the user's next action based on the current state *)
let rec get_action state =
  if is_raised state then (
    print_string "Do you wish to Call, Raise, or Fold?";
    match read_line () with
    | "Call" -> Call
    | "Raise" -> Raise
    | "Fold" -> Fold
    | _ ->
        print_string "Not Valid. Try again";
        get_action state)
  else (
    print_string "Do you wish to Check, Raise, or Fold?";
    match read_line () with
    | "Check" -> Check
    | "Raise" -> Raise
    | "Fold" -> Fold
    | _ ->
        print_string "Not Valid. Try again";
        get_action state)

let string_of_value value =
  match value with
  | Two -> "2"
  | Three -> "3"
  | Four -> "4"
  | Five -> "5"
  | Six -> "6"
  | Seven -> "7"
  | Eight -> "8"
  | Nine -> "9"
  | Ten -> "10"
  | Jack -> "J"
  | Queen -> "Q"
  | King -> "K"
  | Ace -> "A"

let string_of_card (card : Deck.card) =
  string_of_value card.value ^ string_of_suit card.suit

let rec string_of_cards_rec str cards =
  match cards with
  | [] -> str
  | [ h ] -> str ^ " " ^ string_of_card h
  | h :: t ->
      let s = str ^ " " ^ string_of_card h in
      string_of_cards_rec s t

let string_of_cards cards = string_of_cards_rec "" cards

(* Given a state. This prints the user's hand *)
let print_hand hand player =
  let pronoun =
    if player = Player then "Your " else string_of_player player ^ "'s"
  in
  if hand <> [] then
    let s = string_of_cards hand in
    print_string (pronoun ^ "hand is: \n " ^ s ^ "\n")
  else print_string (pronoun ^ "hand is empty. \n")

let print_hands (state : State.state) (player : State.players) =
  if player = Player then print_hand state.users_hand Player
  else
    let rec print_hand_rec acc = function
      | [] -> ()
      | h :: t ->
          print_hand h (Computer acc);
          print_hand_rec (acc + 1) t
    in
    print_hand_rec 1 (Array.to_list state.cpu_hands)

(* Given a state and name of an event *)
let print_event (state : State.state) (event : string) =
  print_string
    ("After the " ^ event ^ " the cards are now: \n"
    ^ string_of_cards state.cards_on_table
    ^ "\n")

let print_win_record (records : win_record list) =
  let rec print_win_rec = function
    | [] -> ()
    | h :: t ->
        print_string
          (string_of_player h.player
          ^ " with a " ^ hand_of_rank h.rank ^ "\n");
        print_win_rec t
  in
  print_win_rec records

let rec reprompt_player_count (num_players : int) : int =
  if num_players > 7 || num_players < 1 then (
    print_string
      "Invalid number of players! Input an integer between 1 and 7: \n";
    reprompt_player_count (read_int ()))
  else num_players


let prompt_bet (state : state) = 
  print_string ("The current bet is "^(string_of_int state.current_bet));
  print_string "Do you wish call, raise, or fold?";
  print_string "How much do you want to bet?";

let rec betting (state : state) stop = 
  let amt = match state.turn with 
  | Player -> bet Player prompt_bet
  | Computer x -> bet (Computer x)
  in
  state.current_bet = amt


let main =
  (* Get Number of players *)
  print_string
    "Welcome to Poker! How many people do you want to play against?\n";
  let num_players = read_int () in
  let num_players = reprompt_player_count num_players in
  let state = active_state num_players in
  delegate state;
  print_hands state Player;
  (* First round of betting will occur here *)
  let utg = (int_of_player state.dealer) mod (1 + Array.length state.cpu_hands)
  in state.turn = utg;

  deal state;
  print_event state "Flop";
  (* Second round of betting will occur here *)
  flop state;
  print_event state "Turn";
  (* Third round of betting will occur here *)
  flop state;
  print_event state "River";
  print_string "You have a ";
  hand_of_rank (List.hd (find_best_hand state Player)).rank ^ "\n"
  |> print_string;
  print_string "The WINNER is....\n";
  print_win_record [ winner state ]
(* print_string "THE FOLLOWING IS FOR DEVELOPMENT ONLY\n"; *)
(* print_string "\nThe other players hands were: \n"; print_win_record
   (find_best_hand state Computer); print_hands state Computer *)

(* Results *)
