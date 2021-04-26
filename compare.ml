open Deck
open State

type win_record = {
  player : players;
  rank : int;
  value : int;
}

let hand_of_rank = function
  | 1 -> "High Card"
  | 2 -> "Pair"
  | 3 -> "Two Pairs"
  | 4 -> "Three of a kind"
  | 5 -> "Straight"
  | 6 -> "Flush"
  | 7 -> "Full House"
  | 8 -> "Four of a kind"
  | 9 -> "Straight Flush"
  | 10 -> "Royal Flush"
  | _ -> "Error. Unknown hand!"

let int_of_val = function
  | Two -> 2
  | Three -> 3
  | Four -> 4
  | Five -> 5
  | Six -> 6
  | Seven -> 7
  | Eight -> 8
  | Nine -> 9
  | Ten -> 10
  | Jack -> 11
  | Queen -> 12
  | King -> 13
  | Ace -> 14

let string_of_suit suit =
  match suit with
  | Spades -> "♠"
  | Hearts -> "♥"
  | Diamonds -> "♦"
  | Clubs -> "♣"

let total_hand (pers_hand : Deck.card list) (table : Deck.card list) =
  pers_hand @ table

type card_check = {
  string_suit : string;
  int_value : int;
}

let rec hand_converter acc (cards : Deck.card list) : card_check list =
  match cards with
  | [] -> acc
  | h :: t ->
      hand_converter
        ({
           string_suit = string_of_suit h.suit;
           int_value = int_of_val h.value;
         }
         :: acc)
        t

let card_compare_int fst_card snd_card =
  compare fst_card.int_value snd_card.int_value

let hand_sort_int (cards : card_check list) =
  List.rev (List.sort card_compare_int cards)

exception GameNotOver

let high_card (cards : card_check list) (user : players) : win_record =
  match hand_sort_int cards with
  | [] -> raise GameNotOver
  | h :: t -> { player = user; rank = 1; value = h.int_value }

let rec one_pair_helper
    (cards : card_check list)
    (user : players)
    (hand_rank : int) : win_record =
  match hand_sort_int cards with
  | h1 :: h2 :: t ->
      if h1.int_value = h2.int_value then
        { player = user; rank = hand_rank; value = h1.int_value }
      else one_pair_helper (h2 :: t) user hand_rank
  | _ -> { player = user; rank = 0; value = 0 }

let one_pair (cards : card_check list) (user : players) =
  one_pair_helper cards user 2

let rec snd_pair_check pair cards user =
  match hand_sort_int cards with
  | h1 :: h2 :: t ->
      if h1.int_value = h2.int_value && pair.value != h1.int_value then
        { player = user; rank = 3; value = h1.int_value }
      else snd_pair_check pair (h2 :: t) user
  | _ -> pair

let two_pair (cards : card_check list) (user : players) : win_record =
  let fst_pair = one_pair cards user in
  snd_pair_check fst_pair cards user

let rec three_kind_helper
    (cards : card_check list)
    (user : players)
    (hand_rank : int) : win_record =
  match hand_sort_int cards with
  | h1 :: h2 :: h3 :: t ->
      if h1.int_value = h2.int_value && h2.int_value = h3.int_value then
        { player = user; rank = hand_rank; value = h1.int_value }
      else three_kind_helper (h2 :: h3 :: t) user hand_rank
  | _ -> { player = user; rank = 0; value = 0 }

let three_kind (cards : card_check list) (user : players) : win_record =
  three_kind_helper cards user 4

let strght_hand_sort_val (cards : card_check list) =
  List.rev (List.sort_uniq card_compare_int cards)

let rec straight (cards : card_check list) (user : players) : win_record
    =
  match strght_hand_sort_val cards with
  | h1 :: h2 :: h3 :: h4 :: h5 :: t ->
      if
        h1.int_value - 1 = h2.int_value
        && h2.int_value - 1 = h3.int_value
        && h3.int_value - 1 = h4.int_value
        && h4.int_value - 1 = h5.int_value
      then { player = user; rank = 5; value = h1.int_value }
      else straight (h2 :: h3 :: h4 :: h5 :: t) user
  | _ -> { player = user; rank = 0; value = 0 }

let rec flush_helper
    (cards : card_check list)
    (user : players)
    (spade_count : int)
    (heart_count : int)
    (diamond_count : int)
    (club_count : int) : win_record =
  match hand_sort_int cards with
  (* FIND THE HIGHEST VALUE IN THE FLUSH *)
  | h :: t ->
      if h.string_suit = "♠" then
        flush_helper t user (spade_count + 1) heart_count diamond_count
          club_count
      else if h.string_suit = "♥" then
        flush_helper t user spade_count (heart_count + 1) diamond_count
          club_count
      else if h.string_suit = "♦" then
        flush_helper t user spade_count heart_count (diamond_count + 1)
          club_count
      else
        flush_helper t user spade_count heart_count diamond_count
          (club_count + 1)
  | [] ->
      if
        spade_count >= 5 || heart_count >= 5 || diamond_count >= 5
        || club_count >= 5
      then { player = user; rank = 6; value = 0 }
      else { player = user; rank = 0; value = 0 }

let flush (cards : card_check list) (user : players) : win_record =
  flush_helper cards user 0 0 0 0

let rec full_house (cards : card_check list) (user : players) :
    win_record =
  match hand_sort_int cards with
  | h1 :: h2 :: h3 :: t
    when h1.int_value = h2.int_value && h2.int_value = h3.int_value ->
      one_pair_helper t user 7
  | h1 :: h2 :: t when h1.int_value = h2.int_value ->
      three_kind_helper t user 7
  | h :: t -> full_house t user
  | _ -> { player = user; rank = 0; value = 0 }

(* let better_full_house (cards : card_check list) (user : players) :
   win_record = let seen = [] in let rec find (card : card_check) list =
   match list with | [] -> -1 | (k, v)::t -> if card.int_value =
   k.int_value then v else lookup value t in let rec loop cards (seen:
   tuple list) = match cards with | [] -> seen |h::t -> if 0 = find h
   seen then loop t acc::(h, 1) else List.map (fun (k,v) -> if k=h then
   (k, v+1) else (k,v)) in let quantity = loop cards *)

let rec four_kind (cards : card_check list) (user : players) :
    win_record =
  match hand_sort_int cards with
  | h1 :: h2 :: h3 :: h4 :: t ->
      if
        h1.int_value = h2.int_value
        && h2.int_value = h3.int_value
        && h3.int_value = h4.int_value
      then { player = user; rank = 8; value = h1.int_value }
      else four_kind (h2 :: h3 :: h4 :: t) user
  | _ -> { player = user; rank = 0; value = 0 }

let rec straight_flush (cards : card_check list) (user : players) :
    win_record =
  match strght_hand_sort_val cards with
  | h1 :: h2 :: h3 :: h4 :: h5 :: t ->
      if
        h1.int_value - 1 = h2.int_value
        && h2.int_value - 1 = h3.int_value
        && h3.int_value - 1 = h4.int_value
        && h4.int_value - 1 = h5.int_value
        && h1.string_suit = h2.string_suit
        && h2.string_suit = h3.string_suit
        && h3.string_suit = h4.string_suit
        && h4.string_suit = h5.string_suit
      then { player = user; rank = 9; value = h1.int_value }
      else straight_flush (h2 :: h3 :: h4 :: h5 :: t) user
  | _ -> { player = user; rank = 0; value = 0 }

let royal_flush (cards : card_check list) (user : players) : win_record
    =
  match strght_hand_sort_val cards with
  | h1 :: h2 :: h3 :: h4 :: h5 :: t ->
      if
        h1.int_value = 14 && h2.int_value = 13 && h3.int_value = 12
        && h4.int_value = 11 && h5.int_value = 10
        && h1.string_suit = "♠" && h2.string_suit = "♠"
        && h3.string_suit = "♠" && h4.string_suit = "♠"
        && h5.string_suit = "♠"
      then { player = user; rank = 10; value = 14 }
      else { player = user; rank = 0; value = 0 }
  | _ -> { player = user; rank = 0; value = 0 }

let best_hand (cards : card_check list) (user : players) : win_record =
  let result = royal_flush cards user in
  if result.rank = 10 then result
  else
    let result = straight_flush cards user in
    if result.rank = 9 then result
    else
      let result = four_kind cards user in
      if result.rank = 8 then result
      else
        let result = full_house cards user in
        if result.rank = 7 then result
        else
          let result = flush cards user in
          if result.rank = 6 then result
          else
            let result = straight cards user in
            if result.rank = 5 then result
            else
              let result = three_kind cards user in
              if result.rank = 4 then result
              else
                let result = two_pair cards user in
                if result.rank = 3 then result
                else
                  let result = one_pair cards user in
                  if result.rank = 2 then result
                  else high_card cards user

let find_best_hand (state : state) (player : players) : win_record list
    =
  let f hand person =
    best_hand
      (hand_converter [] (total_hand hand state.cards_on_table))
      person
  in
  if player = Player then [ f state.users_hand player ]
  else
    let ( -- ) i j =
      let rec aux n acc =
        if n < i then acc else aux (n - 1) (n :: acc)
      in
      aux j []
    in
    let hands = 0 -- (Array.length state.cpu_hands - 1) in
    List.map
      (fun x -> f (Array.get state.cpu_hands x) (Computer 1))
      hands
