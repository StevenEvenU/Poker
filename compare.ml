open Deck

type win_record = {
  player : Table.players;
  rank : int;
  value : int;
}

let int_of_val value =
  match value with
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

let rec hand_converter acc (cards : Deck.card list) =
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

let card_compare fst_card snd_card =
  compare fst_card.int_value snd_card.int_value

let hand_sort (cards : card_check list) = List.sort card_compare cards

exception GameNotOver

let high_card (cards: card_check list) (user : Table.players) : win_record  = 
  match List.rev (hand_sort cards) with
  | [] -> raise GameNotOver
  | h :: t -> {player = user; rank = 1; value = h.int_value}


let one_pair (cards: card_check list) (user: Table.players) 
(* let royal_flush_check pers_hand table = let new_card_list =
   hand_converter [] (total_hand pers_hand table) in let rec check =
   match new_card_list with | h -> *)
