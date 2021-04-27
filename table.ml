open Deck
open Compare
open State

let table_deck = create

let init_state (num : int) =
  {
    users_hand = [];
    cpu_hands = Array.make num [];
    cards_on_table = [];
    deck_rem = table_deck;
    turn = Player;
    user_money = 1000;
    cpu_moneys = Array.make num 1000;
    dealer = Player;
  }

let active_state (num : int) = init_state num

let delegate state =
  Random.self_init ();
  state.deck_rem <- shuffle state.deck_rem (Random.int 100000);
  for j = 0 to Array.length state.cpu_hands - 1 do
    for i = 0 to 1 do
      state.cpu_hands.(j) <-
        (match top_card state.deck_rem with
        | Some card -> card :: state.cpu_hands.(j)
        | none -> state.cpu_hands.(j));
      state.deck_rem <- remove_top state.deck_rem
    done
  done;
  for i = 0 to 1 do
    state.users_hand <-
      (match top_card state.deck_rem with
      | Some card -> card :: state.users_hand
      | none -> state.users_hand);
    state.deck_rem <- remove_top state.deck_rem
  done

let deal state =
  for i = 0 to 2 do
    state.cards_on_table <-
      (match top_card state.deck_rem with
      | Some card -> card :: state.cards_on_table
      | none -> state.cards_on_table);
    state.deck_rem <- remove_top state.deck_rem
  done

let flop state =
  state.cards_on_table <-
    (match top_card state.deck_rem with
    | Some card -> card :: state.cards_on_table
    | none -> state.cards_on_table);
  state.deck_rem <- remove_top state.deck_rem

let bet (state : state) (amt : int) =
  match state.turn with
  | Player -> player_bet amt (state.user_money == amt)
  | Computer x -> comp_bet (Computer x) false

let winner (state : state) : win_record =
  let best_player = find_best_hand state Player in
  let best_computers = find_best_hand state (Computer 1) in
  let hands = List.append best_player best_computers in
  let sf x y =
    if x.rank > y.rank then -1
    else if x.rank = y.rank then
      if x.value > y.value then -1
      else if x.value = y.value then 0
      else 1
    else 1
  in
  let sorted = List.sort sf hands in
  (* List.sort (fun x y -> if x.value > y.value then 1 else -1) *)
  List.hd sorted

(* let round_check state = if List.length state.cards_on_table = 5 then
   winner state else flop state *)

let player_bet amt all_in =
  Pot.add amt all_in;
  amt

let rec index_of element lst acc =
  if lst.(acc) = element then acc else index_of element lst (acc + 1)

let comp_bet comp all_in state =
  let idx = index_of comp state.cpu_hands 0 in
  let max = state.cpu_moneys.(idx) in
  let bet_amt = Random.int max + 1 in
  Pot.add bet_amt all_in;
  bet_amt

let rec distr int_list acc state =
  state.cpu_moneys.(acc) <- int_list.(acc);
  match acc with 8 -> () | _ -> distr int_list (acc + 1) state
