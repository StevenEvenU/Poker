open Deck
open Compare
open State
open Table
open Pot

(* Crates string of the computer's money for printing *)
let comp_arr_to_str str_start arr =
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

(* Creates string of each players' last bet for printing *)
let bets_arr_to_str str_start arr =
  str_start := "";
  for i = 0 to Array.length arr - 1 do
    str_start :=
      !str_start
      ^ str_of_player (player_of_int i)
      ^ "'s last bet:"
      ^ string_of_int arr.(i)
      ^ "\n"
  done;
  !str_start

(* Prints the [amt] that the [player] just bet *)
let print_bet (player : State.players) amt =
  match player with
  | Player -> print_string ("You bet: " ^ string_of_int amt ^ "\n")
  | Computer x ->
      print_string
        ("Opponent " ^ string_of_int x ^ " bet: " ^ string_of_int amt
       ^ "\n")

(* Prints each player's remaining balance *)
let print_bal state =
  print_string ("Your money: " ^ string_of_int state.user_money ^ "\n");
  print_string (comp_arr_to_str (ref "") state.cpu_moneys)

(*** Betting Logic ***)

let next_turn (state : state) players_in current_bet =
  let rec next (state : state) len i =
    (* print_string "index: "; print_int i; print_string "\n"; *)
    if i = len then !players_in.(0)
    else if !players_in.(i) = state.turn then !players_in.(i + 1)
    else next state len (i + 1)
  in
  let next_player = next state (Array.length !players_in - 1) 0 in
  print_string ("Next player: " ^ str_of_player next_player ^ "\n");
  state.turn <- next_player;
  state.current_bet <- current_bet

(* Returns the index of [player] in the [players_in] array. [i] is an
   iterator *)
let rec player_index player players_in i =
  if i >= Array.length !players_in then
    failwith "player not in players_in"
  else if player = !players_in.(i) then i
  else player_index player players_in (i + 1)

(* Safely iterates [num] players ahead in the [players_in] array
   starting from position [idx] *)
let iterate_player idx players_in num =
  print_string
    ("Iterating " ^ string_of_int num ^ " players from index "
   ^ string_of_int idx ^ " with length "
    ^ string_of_int (Array.length !players_in)
    ^ "\n");

  let v =
    if idx + num <= Array.length !players_in - 1 then
      !players_in.(idx + num)
    else !players_in.(0)
  in
  print_string ("Returning player: " ^ str_of_player v ^ "\n");
  v

(* Updates the [bets] array of [player]'s latest [bet] *)
let update_bets bets (player : State.players) state bet =
  (match player with
  | Player -> state.user_money <- state.user_money - bet
  | Computer x ->
      (* print_string ("Computer " ^ string_of_int x ^ "'s money
         updating: " ^ string_of_int state.cpu_moneys.(x - 1) ^ "\n"); *)
      state.cpu_moneys.(x - 1) <- state.cpu_moneys.(x - 1) - bet);
  (* print_string ("Computer money updated: " ^ string_of_int
     state.cpu_moneys.(x - 1) ^ "\n")); *)
  bets.(int_of_player player) <- bet;
  print_bet player bet

let player_prev_bet player bets =
  print_string "DEBUG: \n";
  print_string "----player_prev_bet----\n";
  print_string (bets_arr_to_str (ref "") bets);
  print_string "----^^^^^^^^^^^^^^^----\n";
  bets.(int_of_player player)

let get_money (state : state) = function
  | Player -> state.user_money
  | Computer x -> state.cpu_moneys.(x - 1)

let get_hand (state : state) = function
  | Player -> state.users_hand
  | Computer x -> state.cpu_hands.(x - 1)

(* Valid action functions *)
let valid_check (state : state) player bets =
  (* print_string ("VC - Current Bet: " ^ string_of_int
     state.current_bet ^ "\n"); *)
  let players_bet = player_prev_bet player bets in
  (* print_string ("VC - Players Bet: " ^ string_of_int players_bet ^
     "\n"); *)
  match state.current_bet with
  | 0 -> true
  | x when x != players_bet -> false
  | players_bet when state.current_bet = players_bet -> true
  | _ -> false

let valid_call (state : state) player bets =
  get_money state player
  >= state.current_bet - player_prev_bet player bets

let valid_raise (state : state) player bets =
  match player with
  | Player -> state.user_money > state.current_bet
  | Computer x -> state.cpu_moneys.(x - 1) > state.current_bet

let rec get_raise_amt (state : state) =
  print_string "How much do you wish to raise by? \n";
  let amt = read_int () in
  if valid_raise state Player amt then amt
  else (
    print_string "Invalid amount, please re-enter. \n";
    get_raise_amt state)

(*** Action Functions ***)
let fold_hand state players_in bets =
  let new_players_in = ref [||] in
  let temp = ref 0 in
  for i = 0 to Array.length !players_in - 1 do
    if !players_in.(i) = state.turn then (
      temp := i - 1;
      ())
    else
      new_players_in :=
        Array.append !new_players_in [| !players_in.(i) |]
  done;
  players_in := !new_players_in;
  let l = if !temp < 0 then Array.length !players_in - 1 else !temp in
  state.turn <- !players_in.(l);
  (* prompt_action state players_in bets *)
  state.current_bet

(*** Human Prompt Actions ***)

let rec prompt_action (state : state) players_in bets =
  print_string
    ("The current bet is $" ^ string_of_int state.current_bet ^ "\n");
  print_string "Do you wish check, call, raise, or fold?\n";
  let action = String.uppercase_ascii (read_line ()) in
  human_action action state players_in bets

and human_action action state players_in bets =
  match action with
  | "CHECK" ->
      let amt = human_check state players_in bets in
      Sys.command "clear";
      amt
  | "CALL" ->
      let amt = human_call state players_in bets in
      Sys.command "clear";
      amt
  | "RAISE" ->
      let amt = human_raise state players_in bets in
      Sys.command "clear";
      amt
  | "FOLD" ->
      update_bets bets state.turn state 0;
      fold_hand state players_in bets
  | _ ->
      print_string "Invalid, please try again.\n";
      prompt_action state players_in bets

and human_check (state : state) players_in bets =
  if valid_check state Player bets then state.current_bet
    (* Don't need to update `bets` array *)
  else (
    print_string "You can't check at the moment. Try something else\n";
    prompt_action state players_in bets)

and human_call (state : state) players_in bets =
  if valid_call state Player bets then (
    let amt =
      bet Player (state.current_bet - player_prev_bet Player bets) state
    in
    update_bets bets state.turn state amt;
    amt)
  else 0

and human_raise (state : state) players_in bets =
  let amt = Table.bet Player (get_raise_amt state) state in
  print_string "DEBUG:\n";
  print_string "vvvvvvvvvvvvvvvvvv\n";
  print_string
    ("Prev bet: " ^ string_of_int (player_prev_bet Player bets) ^ "\n");
  print_string "Updating bets: \n";
  update_bets bets state.turn state amt;
  print_string
    ("New bet: " ^ string_of_int (player_prev_bet Player bets) ^ "\n");
  print_string "^^^^^^^^^^^^^^^^^^\n";
  amt

let rec prompt_last_action (state : state) players_in bets =
  print_string "DEBUG: LAST ACTION\n";
  print_string
    ("The current bet is " ^ string_of_int state.current_bet ^ "\n");
  print_string "Do you wish check, call, or fold?\n";

  let action = String.uppercase_ascii (read_line ()) in
  (* print_string (string_of_action action); *)
  (* print_string "matching action \n"; *)
  match action with
  | "CHECK" -> last_human_check state players_in bets
  | "CALL" -> last_human_call state players_in bets
  | "RAISE" ->
      print_string "You can't raise at the moment. Try something else";
      prompt_last_action state players_in bets
  | "FOLD" -> fold_hand state players_in bets
  | _ ->
      print_string "Invalid, please try again.\n";
      prompt_last_action state players_in bets

and last_human_check (state : state) players_in bets =
  print_string
    ("Valid Check: "
    ^ string_of_bool (valid_check state Player bets)
    ^ "\n");
  if valid_check state Player bets then state.current_bet
    (* Don't need to update `bets` array *)
  else (
    print_string "You can't check at the moment. Try something else\n";
    prompt_last_action state players_in bets)

and last_human_call state players_in bets =
  if valid_call state Player bets then (
    print_string
      ("Bet to match is: " ^ string_of_int state.current_bet ^ "\n");
    print_string
      ("Existing bet is: "
      ^ string_of_int (player_prev_bet state.turn bets)
      ^ "\n");
    let amt =
      bet Player (state.current_bet - player_prev_bet Player bets) state
    in
    print_string ("Returned amount is: " ^ string_of_int amt);
    update_bets bets state.turn state amt;
    amt)
  else 0

(*** Computer Prompt Actions ***)

let fold_action (state : state) players_in bets player =
  print_string (str_of_player player ^ " is folding \n");
  update_bets bets state.turn state 0;
  let amt = fold_hand state players_in bets in
  amt

let raise_action (state : state) bets player hand money =
  print_string (str_of_player player ^ " is raising \n");
  let amt = bet player (Float.to_int money) state in
  update_bets bets state.turn state amt;
  print_string ("Raise Amt: " ^ string_of_int amt);
  print_string "\n";
  amt

let check_action (state : state) player =
  (* CHECK This is essentially the helper function for checking *)
  print_string
    (str_of_player player ^ " is checking with current bet:"
    ^ string_of_int state.current_bet
    ^ "\n");
  bet player 0 state

let call_action (state : state) bets player =
  (* CALL *)
  print_string (str_of_player player ^ " is calling \n");
  let amt = bet player state.current_bet state in
  update_bets bets player state amt;
  amt

let fold_after_call (state : state) players_in bets player =
  (* FOLD *)
  print_string
    (str_of_player player ^ " is folding after failing to call \n");
  update_bets bets player state 0;
  fold_hand state players_in bets

let comp_action (state : state) players_in bets =
  let player = state.turn in
  let hand = get_hand state player in
  (* ATTENTION: Uncomment the below line when there is a helper function
     for checking *)
  if 1 = Array.length !players_in then check_action state player
  else
    let p = Probability.prob (hand @ state.cards_on_table) 10 in
    let v = p *. Float.of_int (get_money state player) in
    match v with
    | v when v < 0.9 *. Float.of_int state.current_bet ->
        fold_action state players_in bets player
    | v
      when v > 1.1 *. Float.of_int state.current_bet
           && valid_raise state player bets ->
        raise_action state bets player hand v
    | _ ->
        if valid_check state player bets then check_action state player
        else if valid_call state player bets then
          call_action state bets player
        else fold_after_call state players_in bets player

(*** Recursive Betting Round ***)
let rec rec_bet_round (state : state) players_in bets plays =
  print_string "Current Balances: \n";
  print_bal state;
  print_string "Current Bets: \n";
  player_prev_bet state.turn bets;
  if state.turn = !players_in.(0) && plays > 0 then
    final_play state players_in bets plays
  else play state players_in bets plays

and final_play_player state players_in bets plays =
  let amt = prompt_last_action state players_in bets in
  amt

and final_play_cpu state players_in bets plays cpu_int =
  let amt = comp_action state players_in bets in
  amt

and final_play state players_in bets plays =
  let player = state.turn in

  (* print_string (str_of_player player ^ "'s final turn\n"); *)
  match player with
  | Player -> final_play_player state players_in bets plays
  | Computer x -> final_play_cpu state players_in bets plays x

and play_player state players_in bets plays =
  let amt = prompt_action state players_in bets in
  print_string ("play_player amt: " ^ string_of_int amt ^ "\n");
  amt

and play_cpu state players_in bets plays cpu_int =
  let amt = comp_action state players_in bets in
  amt

and play state players_in bets plays =
  let player = state.turn in
  print_string (">>> " ^ str_of_player player ^ "'s turn\n");
  let player_count = Array.length !players_in in
  let turn_index = player_index player players_in 0 in
  (* print_string ("Player index is: "^(string_of_int turn_index)^"\n"); *)
  let amt =
    match player with
    | Player -> play_player state players_in bets plays
    | Computer x -> play_cpu state players_in bets plays x
  in
  (* Update state.turn and state.current_bet *)
  print_string ("Setting current_bet to: " ^ string_of_int amt ^ "\n");
  state.current_bet <- amt;
  if player_count = Array.length !players_in then
    state.turn <- iterate_player turn_index players_in 1
  else state.turn <- iterate_player turn_index players_in 0;
  rec_bet_round state players_in bets (plays + 1)

let last_call state players_in bets =
  let max = Array.fold_left max 0 bets in
  (* print_string ("Max bet is: " ^ string_of_int max ^ "\n"); *)
  for i = 0 to Array.length !players_in - 2 do
    let idx = player_index state.turn players_in 0 in
    state.turn <- !players_in.(idx + 1);
    let residual = max - player_prev_bet state.turn bets in
    print_string
      (str_of_player state.turn
      ^ "calling before with an\n       additional: "
      ^ string_of_int residual ^ "\n");
    let amt = bet state.turn residual state in
    print_string
      (str_of_player state.turn
      ^ "calling with an\n       additional: " ^ string_of_int amt
      ^ "\n");
    update_bets bets state.turn state residual;
    ()
  done;
  next_turn state players_in state.current_bet
