open Deck
open Compare
open State
open Table

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

(*** Betting Logic ***)

let rec get_action input =
  match String.uppercase_ascii input with
  | "CHECK" -> Check
  | "CALL" -> Call
  | "RAISE" -> Raise
  | "FOLD" -> Fold
  | _ ->
      print_string "Invalid, please try again.";
      get_action (read_line ())

let next_turn (state : state) players_in current_bet =
  let next_player =
    let len = Array.length players_in - 1 in
    for i = 0 to len do
      if !(players_in.(i)) = state.turn then i + 1
      else if i = len then 0
    done
  in
  state.turn <- player_of_int next_player;
  state.current_bet <- current_bet

let update_bets bets (player : State.players) bet =
  bets.(int_of_player player) <- bet

let player_prev_bet (state : state) bets =
  bets.(int_of_player state.turn)

let get_money (state : state) player =
  match player with
  | Player -> state.user_money
  | Computer x -> state.cpu_moneys.(x - 1)

let valid_check (state : state) bets =
  let players_bet = player_prev_bet state bets in
  match state.current_bet with
  | 0 -> true
  | players_bet -> true
  | _ -> false

let valid_call (state : state) bets =
  get_money state.turn <= state.current_bet

(* TODO: Implement. Make sure they have the funds to raise*)
let valid_raise (state : state) bets =
  let curr_player = state.turn in
  match curr_player with
  | Player -> state.user_money > state.current_bet
  | Computer x -> state.cpu_moneys.(x - 1) > state.current_bet

let rec get_raise_amount (state : state) =
  print_string "How much do you wish to raise by? \n";
  let amt = read_int () in
  if valid_raise state amt then amt
  else (
    print_string "Invalid amount, please re-enter. \n";
    get_raise_amount state)

(* TODO: Only prompt available actions, not all *)
let rec prompt_action (state : state) bets =
  print_string ("The current bet is " ^ string_of_int state.current_bet);
  print_string "Do you wish check, call, raise, or fold?";

  let action = get_action (read_line ()) in
  match action with
  | Check ->
      if valid_check state bets then state.current_bet
        (* Don't need to update `bets` array *)
      else (
        print_string "You can't check at the moment. Try something else";
        prompt_action state bets)
  | Call ->
      if valid_call state bets then (
        let amt = Table.bet Player state.current_bet in
        update_bets state.turn amt;
        amt)
      else 0
  | Raise ->
      let amt = bet Player (get_raise_amount state) state in
      update_bets state.turn amt;
      amt
  | Fold ->
      let new_player_in = ref [||] in
      for i = 0 to Array.length players_in do
        if player_in.(i) = state.turn then ()
        else new_players_in := Array.append new_players_in player_in.(i)
      done;
      players_in := new_players_in;
      update_bets state.turn - 1;
      state.current_bet

let rec rec_betting_round (state : state) players_in bets =
  let player = state.turn in
  let amt =
    match player with
    | Player -> prompt_action bets
    | Computer x -> bet (Computer x) 0 state
  in
  (* Update state.turn and state.current_bet *)
  next_turn state players_in amt

let betting_round (state : state) players_in =
  (* bets is the list of how much each player has bet so far in each
     round *)
  let bets = Array.make (1 + Array.length state.cpu_hands) 0 in
  rec_betting_round state players_in bets

let main =
  (* Get Number of players *)
  print_string
    "Welcome to Poker! How many people do you want to play against?\n";
  let num_players = read_int () in
  let num_players = reprompt_player_count num_players in
  let state = active_state num_players in
  delegate state;
  let players_in = ref [| Player |] in
  for x = 1 to num_players do
    players_in := Array.append !players_in [| Computer x |]
  done;
  print_hands state Player;
  (* First round of betting will occur here *)
  let utg =
    int_of_player state.dealer mod (1 + Array.length state.cpu_hands)
  in
  state.turn = utg;

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
