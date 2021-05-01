open Deck
open Compare
open State
open Table
open Pot

let players_to_string = function
  | Player -> "Player "
  | Computer x -> "Computer " ^ string_of_int x

let stupid str_start arr min max =
  str_start := "[|" ^ players_to_string !arr.(min);
  for i = min + 1 to max do
    str_start := !str_start ^ "," ^ players_to_string !arr.(i)
  done;
  str_start := !str_start ^ "|]";
  !str_start

(** Types of user action *)
type action =
  | Check
  | Call
  | Raise
  | Fold

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

let string_of_action = function
  | Check -> "Check"
  | Call -> "Call"
  | Raise -> "Raise"
  | Fold -> "Fold"

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

let print_bet (player : State.players) amt =
  match player with
  | Player -> print_string ("You bet: " ^ string_of_int amt ^ "\n")
  | Computer x ->
      print_string
        ("Opponent " ^ string_of_int x ^ " bet: " ^ string_of_int amt
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

let next_turn (state : state) players_in current_bet =
  let rec next (state : state) len i =
    if i = len then !players_in.(0)
    else if !players_in.(i) = state.turn then !players_in.(i + 1)
    else next state len (i + 1)
  in
  let next_player = next state (Array.length !players_in - 1) 0 in
  state.turn <- next_player;
  state.current_bet <- current_bet + state.current_bet

let update_bets bets (player : State.players) state bet =
  (match player with
  | Player -> state.user_money <- state.user_money - bet
  | Computer x ->
      state.cpu_moneys.(x - 1) <- state.cpu_moneys.(x - 1) - bet);
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
  | x when x != players_bet -> false
  | players_bet when state.current_bet = players_bet -> true
  | _ -> false

let valid_call (state : state) bets =
  get_money state state.turn >= state.current_bet

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
let rec prompt_action (state : state) players_in bets =
  print_string
    ("The current bet is $" ^ string_of_int state.current_bet ^ "\n");
  print_string "Do you wish check, call, raise, or fold?\n";

  let action = String.uppercase_ascii (read_line ()) in
  match action with
  | "CHECK" ->
      if valid_check state bets then state.current_bet
        (* Don't need to update `bets` array *)
      else (
        print_string
          "You can't check at the moment. Try something else\n";
        prompt_action state players_in bets)
  | "CALL" ->
      if valid_call state bets then (
        let amt =
          bet Player
            (state.current_bet - player_prev_bet state bets)
            state
        in
        update_bets bets state.turn state amt;
        amt)
      else 0
  | "RAISE" ->
      let amt = Table.bet Player (get_raise_amount state) state in
      update_bets bets state.turn state amt;
      amt
  | "FOLD" ->
      print_string
        "You can't fold at the moment. Yet to be implemented\n";
      (* let new_players_in = ref [||] in print_string "defined\n\n
         new_players_in"; for i = 0 to Array.length !players_in - 1 do
         if !players_in.(i) = state.turn then () else new_players_in :=
         Array.append !new_players_in [| !players_in.(i) |] done;
         players_in := !new_players_in; print_string "reset defined\n
         players_in"; update_bets bets state.turn (-1); print_string
         "updated bets"; *)
      prompt_action state players_in bets
  | _ ->
      print_string "Invalid, please try again.\n";
      prompt_action state players_in bets

let rec prompt_last_action (state : state) players_in bets =
  print_string
    ("The current bet is " ^ string_of_int state.current_bet ^ "\n");
  print_string "Do you wish check, call, or fold?\n";

  let action = String.uppercase_ascii (read_line ()) in
  (* print_string (string_of_action action); *)
  (* print_string "matching action \n"; *)
  match action with
  | "CHECK" ->
      print_string
        ("Valid Check: "
        ^ string_of_bool (valid_check state bets)
        ^ "\n");
      if valid_check state bets then state.current_bet
        (* Don't need to update `bets` array *)
      else (
        print_string
          "You can't check at the moment. Try something else\n";
        prompt_action state players_in bets)
  | "CALL" ->
      if valid_call state bets then (
        let amt =
          bet Player
            (state.current_bet - player_prev_bet state bets)
            state
        in
        update_bets bets state.turn state amt;
        amt)
      else 0
  | "RAISE" ->
      print_string "You can't raise at the moment. Try something else";
      prompt_last_action state players_in bets
  | "FOLD" ->
      print_string
        "You can't fold at the moment. Yet to be implemented\n";
      (* let new_players_in = ref [||] in print_string "defined
         new_players_in\n"; for i = 0 to Array.length !players_in - 1 do
         if !players_in.(i) = state.turn then () else new_players_in :=
         Array.append !new_players_in [| !players_in.(i) |] done;
         print_string (stupid (ref "") players_in 0 (Array.length
         !players_in - 1)); players_in := !new_players_in; print_string
         "reset defined players_in\n"; update_bets bets state.turn (-1);
         print_string "updated bets\n"; *)
      prompt_last_action state players_in bets
  | _ ->
      print_string "Invalid, please try again.\n";
      prompt_last_action state players_in bets

let rec rec_betting_round
    (state : state)
    players_in
    bets
    starting_player
    plays =
  if state.turn = starting_player && plays > 0 then
    let player = state.turn in
    match player with
    | Player -> prompt_last_action state players_in bets
    | Computer x -> bet (Computer x) 0 state
  else
    let player = state.turn in
    let amt =
      match player with
      | Player ->
          let amt = prompt_action state players_in bets in
          print_bet Player amt;
          amt
      | Computer x ->
          let amt = bet (Computer x) 0 state in
          print_bet (Computer x) amt;
          amt
    in
    (* Update state.turn and state.current_bet *)
    next_turn state players_in amt;
    rec_betting_round state players_in bets starting_player (plays + 1)

let betting_round (state : state) players_in =
  (* bets is the list of how much each player has bet so far in each
     round *)
  let bets = Array.make (1 + Array.length state.cpu_hands) 0 in
  rec_betting_round state players_in bets state.turn 0;
  ()

let filter_win_rec_list win_rec_list players_in =
  let rec playing player players_in i =
    if i = Array.length players_in then false
    else if players_in.(i) = player then true
    else playing player players_in (i + 1)
  in
  List.filter
    (fun (x : win_record) -> playing x.player players_in 0)
    win_rec_list

(***** ***** MAIN GAME OPERATION ***** *****)

let main =
  (* Get Number of players *)
  print_string
    "Welcome to Poker! How many people do you want to play against?\n";
  let num_players = read_int () in
  let num_players = reprompt_player_count num_players in
  let state = active_state num_players in
  delegate state;

  (* Delegates cards to players *)
  let players_in = ref [| Player |] in
  for x = 1 to num_players do
    players_in := Array.append !players_in [| Computer x |]
  done;
  print_hands state Player;
  print_string
    ("You currently have $"
    ^ string_of_int state.user_money
    ^ " to gamble.\n");

  (* Set the first player to the under the gun (utg) *)
  let utg =
    int_of_player state.dealer mod (1 + Array.length state.cpu_hands)
  in
  state.turn <- player_of_int utg;

  (* First round of betting will occur here *)
  betting_round (state : state) players_in;

  deal state;
  print_event state "Flop";

  (* Second round of betting will occur here *)
  betting_round (state : state) players_in;

  flop state;
  print_event state "Turn";

  (* Third round of betting will occur here *)
  betting_round (state : state) players_in;

  flop state;
  print_event state "River";
  print_string "You have a ";
  hand_of_rank (List.hd (find_best_hand state Player)).rank ^ "\n"
  |> print_string;
  print_string "The WINNER is....\n";
  let win_record_list = [ winner state ] in
  print_win_record win_record_list;

  to_winner (filter_win_rec_list win_record_list !players_in)
(* print_string "THE FOLLOWING IS FOR DEVELOPMENT ONLY\n"; *)
(* print_string "\nThe other players hands were: \n"; print_win_record
   (find_best_hand state Computer); print_hands state Computer *)

(* Results *)
