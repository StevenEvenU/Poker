open Deck
open Compare
open State
open Table
open Pot

(* let stupid str_start arr min max = str_start := "[|" ^
   string_of_player !arr.(min); for i = min + 1 to max do str_start :=
   !str_start ^ "," ^ string_of_player !arr.(i) done; str_start :=
   !str_start ^ "|]"; !str_start *)

let print_bet (player : State.players) amt =
  match player with
  | Player -> print_string ("You bet: " ^ string_of_int amt ^ "\n")
  | Computer x ->
      print_string
        ("Opponent " ^ string_of_int x ^ " bet: " ^ string_of_int amt
       ^ "\n")

(*** Betting Logic ***)

let next_turn (state : state) players_in current_bet =
  let rec next (state : state) len i =
    if i = len then !players_in.(0)
    else if !players_in.(i) = state.turn then !players_in.(i + 1)
    else next state len (i + 1)
  in
  let next_player = next state (Array.length !players_in - 1) 0 in
  state.turn <- next_player;
  state.current_bet <- current_bet

let update_bets bets (player : State.players) state bet =
  (match player with
  | Player -> state.user_money <- state.user_money - bet
  | Computer x ->
      (* print_string ("Computer " ^ string_of_int x ^ "'s money
         updating: " ^ string_of_int state.cpu_moneys.(x - 1) ^ "\n"); *)
      state.cpu_moneys.(x - 1) <- state.cpu_moneys.(x - 1) - bet);
  (* print_string ("Computer money updated: " ^ string_of_int
     state.cpu_moneys.(x - 1) ^ "\n")); *)
  bets.(int_of_player player) <- bet

let player_prev_bet (state : state) bets =
  bets.(int_of_player state.turn)

let get_money (state : state) player =
  match player with
  | Player -> state.user_money
  | Computer x -> state.cpu_moneys.(x - 1)

(* Valid action functions *)
let valid_check (state : state) bets =
  (* print_string ("VC - Current Bet: " ^ string_of_int
     state.current_bet ^ "\n"); *)
  let players_bet = player_prev_bet state bets in
  (* print_string ("VC - Players Bet: " ^ string_of_int players_bet ^
     "\n"); *)
  match state.current_bet with
  | 0 -> true
  | x when x != players_bet -> false
  | players_bet when state.current_bet = players_bet -> true
  | _ -> false

let valid_call (state : state) bets =
  get_money state state.turn
  >= state.current_bet - player_prev_bet state bets

let valid_raise (state : state) bets =
  let curr_player = state.turn in
  match curr_player with
  | Player -> state.user_money > state.current_bet
  | Computer x -> state.cpu_moneys.(x - 1) > state.current_bet

let rec get_raise_amt (state : state) =
  print_string "How much do you wish to raise by? \n";
  let amt = read_int () in
  if valid_raise state amt then amt
  else (
    print_string "Invalid amount, please re-enter. \n";
    get_raise_amt state)

(* Prompt actions *)

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
      let amt = Table.bet Player (get_raise_amt state) state in
      update_bets bets state.turn state amt;
      amt
  | "FOLD" ->
      let new_players_in = ref [||] in
      (* print_string "defined\n\n new_players_in"; *)
      for i = 0 to Array.length !players_in - 1 do
        if !players_in.(i) = state.turn then ()
        else
          new_players_in :=
            Array.append !new_players_in [| !players_in.(i) |]
      done;
      players_in := !new_players_in;
      (* print_string "reset defined\n players_in"; *)
      update_bets bets state.turn state 0;
      (* print_string "updated bets"; *)
      (* prompt_action state players_in bets *)
      state.current_bet
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
        prompt_last_action state players_in bets)
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
      let new_players_in = ref [||] in
      (* print_string "defined new_players_in\n"; *)
      for i = 0 to Array.length !players_in - 1 do
        if !players_in.(i) = state.turn then ()
        else
          new_players_in :=
            Array.append !new_players_in [| !players_in.(i) |]
      done;
      (* print_string (stupid (ref "") players_in 0 (Array.length
         !players_in - 1)); *)
      players_in := !new_players_in;
      (* print_string "reset defined players_in\n"; update_bets bets
         state.turn state (-1); *)
      (* print_string "updated bets\n"; *)
      state.current_bet
      (* prompt_last_action state players_in bets *)
  | _ ->
      print_string "Invalid, please try again.\n";
      prompt_last_action state players_in bets

let rec bet_round (state : state) players_in bets plays =
  if state.turn = !players_in.(0) && plays > 0 then (
    let player = state.turn in
    (* print_string (string_of_player player ^ "'s final turn\n"); *)
    match player with
    | Player ->
        let amt = prompt_last_action state players_in bets in
        (* print_bet Player amt; *)
        amt
    | Computer x ->
        (* print_string "Before updating bets \n"; *)
        let amt = bet (Computer x) 0 state in
        (* print_string "Updating bets\n"; *)
        update_bets bets (Computer x) state amt;
        (* print_bet (Computer x) amt; *)
        amt)
  else
    let player = state.turn in
    print_string (string_of_player player ^ "'s turn\n");
    let amt =
      match player with
      | Player ->
          let amt = prompt_action state players_in bets in
          (* print_bet Player amt; *)
          amt
      | Computer x ->
          let amt = bet (Computer x) 0 state in
          update_bets bets (Computer x) state amt;
          (* print_bet (Computer x) amt; *)
          amt
    in
    (* Update state.turn and state.current_bet *)
    next_turn state players_in amt;
    bet_round state players_in bets (plays + 1)

let last_call state players_in bets =
  let max = Array.fold_left max 0 bets in
  (* print_string ("Max bet is: " ^ string_of_int max ^ "\n"); *)
  for i = 0 to Array.length !players_in - 2 do
    next_turn state players_in state.current_bet;
    let test = max - player_prev_bet state bets in
    (* print_string (string_of_player state.turn ^ "calling before with
       an additional: " ^ string_of_int test ^ "\n"); *)
    let amt = bet_specific state.turn test in
    (* print_string (string_of_player state.turn ^ "calling with an
       additional: " ^ string_of_int amt ^ "\n"); update_bets bets
       state.turn state test; *)
    ()
  done;
  next_turn state players_in state.current_bet
