open Deck
open Compare
open State
open Table
open Pot
open Betting

let stupid str_start arr min max =
  str_start := "[|" ^ string_of_player !arr.(min);
  for i = min + 1 to max do
    str_start := !str_start ^ "," ^ string_of_player !arr.(i)
  done;
  str_start := !str_start ^ "|]";
  !str_start

let arr_to_string str_start arr =
  str_start := "";
  for i = 0 to Array.length arr - 1 do
    str_start :=
      !str_start ^ "Computer "
      ^ string_of_int (i + 1)
      ^ "'s money:"
      ^ string_of_int arr.(i)
      ^ "\n"
  done;
  !str_start

let rec list_to_string str_start lst =
  match lst with
  | [] -> str_start ^ "]"
  | h :: t -> list_to_string (str_start ^ h ^ ", ") t

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


let print_balances state =
  print_string ("Your money: " ^ string_of_int state.user_money ^ "\n");
  print_string (arr_to_string (ref "") state.cpu_moneys)

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

let betting_round (state : state) players_in =
  (* bets is the list of how much each player has bet so far in each
     round *)
  print_string
    ("Starting with player: " ^ string_of_player state.turn ^ "\n");
  let bets = Array.make (1 + Array.length state.cpu_hands) 0 in
  let amt = rec_betting_round state players_in bets 0 in
  Betting.last_call state players_in bets;
  state.current_bet <- 0

let filter_win_rec_list win_rec_list players_in =
  print_string "Filtering the following list: \n";
  print_win_record win_rec_list;
  print_string "The players still in are: ";
  print_string
    (stupid (ref "") players_in 0 (Array.length !players_in - 1));
  print_string "Done\n";
  let rec playing player players_in i =
    if i >= Array.length players_in then false
    else if players_in.(i) = player then true
    else playing player players_in (i + 1)
  in
  List.filter
    (fun (x : win_record) ->
      let p = playing x.player !players_in 0 in
      p)
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
  Sys.command("clear");

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
  print_string "\n First Betting Round is Starting\n";
  betting_round (state : state) players_in;
  Sys.command("clear");
  print_string "Players in: \n";
  print_string
    (stupid (ref "") players_in 0 (Array.length !players_in - 1));
  print_balances state;
  print_string "\n";
  print_string "\n Current amount in pot is: \n";
  print_string (print_pot ());

  deal state;
  print_event state "Flop";

  (* Second round of betting will occur here *)
  print_string "Second Betting Round\n";
  betting_round (state : state) players_in;
  (* Sys.command("clear"); *)
  print_string "Players in: \n";
  print_string
    (stupid (ref "") players_in 0 (Array.length !players_in - 1));
  print_balances state;
  print_string "\n";
  print_string "\n Current amount in pot is: \n";
  print_string (print_pot ());

  flop state;
  print_event state "Turn";

  (* Third round of betting will occur here *)
  print_string "Third Betting Round\n";
  betting_round (state : state) players_in;
  print_string "Players in: \n";
  print_string
    (stupid (ref "") players_in 0 (Array.length !players_in - 1));
  print_balances state;
  print_string "\n";
  print_string "\n Current amount in pot is: \n";
  print_string (print_pot ());

  flop state;
  print_event state "River";
  print_string "You have a ";
  hand_of_rank (List.hd (find_best_hand state Player)).rank ^ "\n"
  |> print_string;
  print_string "The WINNER is....\n";
  let win_record_list = winner state in
  let filtered_winners =
    filter_win_rec_list win_record_list players_in
  in
  print_string
    ("Players count: " ^ string_of_int (Array.length !players_in) ^ "\n");
  print_string "tesatfdjlk: ";
  print_int (List.length filtered_winners);
  print_string "\n";
  print_win_record filtered_winners;

  let pot_array = to_winner filtered_winners state in
  print_string (arr_to_string (ref "") pot_array);
  print_balances state;
  distr pot_array state (List.length win_record_list - 1);
  print_balances state
(* print_string "THE FOLLOWING IS FOR DEVELOPMENT ONLY\n"; *)
(* print_string "\nThe other players hands were: \n"; print_win_record
   (find_best_hand state Computer); print_hands state Computer *)

(* Results *)
