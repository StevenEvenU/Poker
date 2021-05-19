open State
open Table
open Deck
open Pot

let deck_remover (lst : Deck.card option list) =
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

let prob (lst : Deck.card list) (num : int) =
  let deck_left = ref (deck_remover (List.map (fun a -> Some a) lst)) in
  let hand_cards = [ List.hd lst; List.hd (List.tl lst) ] in
  let table_cards = List.tl (List.tl lst) in
  let cpu_cards = Array.make num [ { suit = Clubs; value = Eight } ] in
  let c1 = ref { suit = Clubs; value = Eight } in
  let c2 = ref { suit = Clubs; value = Eight } in
  for i = 0 to num do
    (c1 :=
       match Deck.top_card !deck_left with
       | Some c -> c
       | _ -> failwith "oops");
    deck_left := Deck.remove_top !deck_left;
    (c2 :=
       match Deck.top_card !deck_left with
       | Some c -> c
       | _ -> failwith "oops");
    deck_left := Deck.remove_top !deck_left;
    cpu_cards.(i) <- [ !c1; !c2 ]
  done;
  let state_1 =
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
  in
  let win_rec = Table.winner state_1 in
  let win_lst = Pot.top_winners win_rec in
  if List.mem 0 win_lst then 1.0 else 0.0
