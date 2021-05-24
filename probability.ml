open State
open Table
open Deck
open Pot

let repitions = 100

let deck_remov (lst : Deck.card option list) =
  let deck0 = Deck.create in
  let rec deck_rec lst0 deck1 =
    match lst0 with
    | [] -> deck1
    | h :: t ->
        deck_rec t
          (List.filter (fun a -> if a != h then true else false) deck1)
  in
  let deck1 = deck_rec lst deck0 in
  deck1

let de_opt card_opt =
  match card_opt with Some c -> c | _ -> failwith "should not happen"

let cpu_cards_helper cpu_cards c1 c2 deck_left num =
  deck_left := Deck.shuffle !deck_left;
  for i = 0 to num - 1 do
    c1 := de_opt (Deck.top_card !deck_left);
    deck_left := Deck.remove_top !deck_left;
    c2 := de_opt (Deck.top_card !deck_left);
    deck_left := Deck.remove_top !deck_left;
    cpu_cards.(i) <- [ !c1; !c2 ]
  done

let make_state hand_cards cpu_cards table_cards deck_left num =
  {
    users_hand = hand_cards;
    cpu_hands = cpu_cards;
    cards_on_table = table_cards;
    deck_rem = !deck_left;
    turn = Player;
    user_money = 1000;
    cpu_moneys = Array.make num 1000;
    dealer = Player;
    current_bet = 0;
  }

let rec rand_card card deck_left =
  card := de_opt (Deck.top_card !deck_left);
  deck_left := Deck.remove_top !deck_left;
  card

let rec table_fill lst deck_left =
  deck_left := Deck.shuffle !deck_left;
  let card = ref { suit = Clubs; value = Eight } in
  if List.length lst < 3 then
    let card = rand_card card deck_left in
    table_fill (!card :: lst) deck_left
  else lst

let rec prob_helper lst num acc count =
  if count = repitions then float_of_int acc /. float_of_int repitions
  else
    let deck_left = ref (deck_remov (List.map (fun a -> Some a) lst)) in
    let hand_cards = [ List.hd lst; List.hd (List.tl lst) ] in
    let table_cards = table_fill (List.tl (List.tl lst)) deck_left in
    let cpu_cards =
      Array.make num [ { suit = Clubs; value = Eight } ]
    in
    let c1 = ref { suit = Clubs; value = Eight } in
    let c2 = ref { suit = Clubs; value = Eight } in
    cpu_cards_helper cpu_cards c1 c2 deck_left num;
    let state_1 =
      make_state hand_cards cpu_cards table_cards deck_left num
    in
    let win_rec = Table.winner state_1 in
    let win_lst = Pot.top_winners win_rec in
    if List.mem 0 win_lst then prob_helper lst num (acc + 1) (count + 1)
    else prob_helper lst num acc (count + 1)

let prob (lst : Deck.card list) (num : int) = prob_helper lst num 0 0

let print_prob lst num =
  Printf.printf "Probability: %fs\n" (prob_helper lst num 0 0)
