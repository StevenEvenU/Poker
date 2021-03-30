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

let card_compare_int fst_card snd_card =
  compare fst_card.int_value snd_card.int_value

let hand_sort_int (cards : card_check list) =
  List.rev (List.sort card_compare_int cards)

let card_compare_str fst_card snd_card =
  compare fst_card.string_suit snd_card.string_suit

let hand_sort_str (cards : card_check list) =
  List.rev (List.sort card_compare_str cards)

exception GameNotOver

let high_card (cards : card_check list) (user : Table.players) :
    win_record =
  match hand_sort_int cards with
  | [] -> raise GameNotOver
  | h :: t -> { player = user; rank = 1; value = h.int_value }

let rec one_pair_helper
    (cards : card_check list)
    (user : Table.players)
    (value : int)
    (hand_rank : int) : win_record =
  match hand_sort_int cards with
  | h1 :: h2 :: t ->
      if h1.int_value = h2.int_value then
        { player = user; rank = hand_rank; value = h1.int_value }
      else one_pair_helper (h2 :: t) user value hand_rank
  | _ -> { player = user; rank = 0; value = 0 }

let one_pair
    (cards : card_check list)
    (user : Table.players)
    (value : int) =
  one_pair_helper cards user value 2

let rec snd_pair_check pair cards user value =
  match hand_sort_int cards with
  | h1 :: h2 :: t ->
      if h1.int_value = h2.int_value && pair.value != h1.int_value then
        { player = user; rank = 3; value = h1.int_value }
      else snd_pair_check pair (h2 :: t) user value
  | _ -> pair

let two_pair
    (cards : card_check list)
    (user : Table.players)
    (value : int) : win_record =
  let fst_pair = one_pair cards user value in
  snd_pair_check fst_pair cards user value

let rec three_kind
    (cards : card_check list)
    (user : Table.players)
    (value : int) : win_record =
  match hand_sort_int cards with
  | h1 :: h2 :: h3 :: t ->
      if h1.int_value = h2.int_value && h2.int_value = h3.int_value then
        { player = user; rank = 4; value = h1.int_value }
      else three_kind (h2 :: h3 :: t) user value
  | _ -> { player = user; rank = 0; value = 0 }

let strght_hand_sort_val (cards : card_check list) =
  List.rev (List.sort_uniq card_compare_int cards)

let rec straight
    (cards : card_check list)
    (user : Table.players)
    (value : int) : win_record =
  match strght_hand_sort_val cards with
  | h1 :: h2 :: h3 :: h4 :: h5 :: t ->
      if
        h1.int_value - 1 = h2.int_value
        && h2.int_value - 1 = h3.int_value
        && h3.int_value - 1 = h4.int_value
        && h4.int_value - 1 = h5.int_value
      then { player = user; rank = 5; value = h1.int_value }
      else straight (h2 :: h3 :: h4 :: h5 :: t) user value
  | _ -> { player = user; rank = 0; value = 0 }

let rec flush_helper
    (cards : card_check list)
    (user : Table.players)
    (value : int)
    (spade_count : int)
    (heart_count : int)
    (diamond_count : int)
    (club_count : int) : win_record =
  match hand_sort_int cards with
  (* FIND THE HIGHEST VALUE IN THE FLUSH *)
  | h :: t ->
      if h.string_suit = "♠" then
        flush_helper t user value (spade_count + 1) heart_count
          diamond_count club_count
      else if h.string_suit = "♥" then
        flush_helper t user value spade_count (heart_count + 1)
          diamond_count club_count
      else if h.string_suit = "♦" then
        flush_helper t user value spade_count heart_count
          (diamond_count + 1) club_count
      else
        flush_helper t user value spade_count heart_count diamond_count
          (club_count + 1)
  | [] ->
      if
        spade_count >= 5 || heart_count >= 5 || diamond_count >= 5
        || club_count >= 5
      then { player = user; rank = 6; value = 0 }
      else { player = user; rank = 0; value = 0 }

let flush
    (cards : card_check list)
    (user : Table.players)
    (value : int)
    (spade_count : int)
    (heart_count : int)
    (diamond_count : int)
    (club_count : int) : win_record =
  flush_helper cards user value 0 0 0 0

let rec full_house
    (cards : card_check list)
    (user : Table.players)
    (value : int) : win_record =
  match hand_sort_int cards with
  | h1 :: h2 :: h3 :: t ->
      if h1.int_value = h2.int_value && h2.int_value = h3.int_value then
        one_pair_helper cards user value 7
      else full_house (h2 :: h3 :: t) user value
  | _ -> { player = user; rank = 0; value = 0 }

let rec four_kind
    (cards : card_check list)
    (user : Table.players)
    (value : int) : win_record =
  match hand_sort_int cards with
  | h1 :: h2 :: h3 :: h4 :: t ->
      if
        h1.int_value = h2.int_value
        && h2.int_value = h3.int_value
        && h3.int_value = h4.int_value
      then { player = user; rank = 8; value = h1.int_value }
      else four_kind (h2 :: h3 :: h4 :: t) user value
  | _ -> { player = user; rank = 0; value = 0 }

let rec straight_flush
    (cards : card_check list)
    (user : Table.players)
    (value : int) : win_record =
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
      else straight_flush (h2 :: h3 :: h4 :: h5 :: t) user value
  | _ -> { player = user; rank = 0; value = 0 }

let royal_flush = failwith "Not Implemented"
