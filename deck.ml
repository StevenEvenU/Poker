type suit =
  | Spades
  | Hearts
  | Diamonds
  | Clubs

type value =
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace

type card = {
  suit : suit;
  value : value;
}

type deck = card option list

let deck_size = 52

let to_deck (c_o_l : card option list) : deck = c_o_l

let create_card (valu : value) (sut : suit) =
  { suit = sut; value = valu }

let rec create_helper (deck1 : card option list) (place : int) =
  if place < deck_size then
    let sut =
      if place < 13 then Hearts
      else if place < 26 then Diamonds
      else if place < 39 then Spades
      else Clubs
    in
    let valu =
      if place mod 13 = 0 then Two
      else if place mod 13 = 1 then Three
      else if place mod 13 = 2 then Four
      else if place mod 13 = 3 then Five
      else if place mod 13 = 4 then Six
      else if place mod 13 = 5 then Seven
      else if place mod 13 = 6 then Eight
      else if place mod 13 = 7 then Nine
      else if place mod 13 = 8 then Ten
      else if place mod 13 = 9 then Jack
      else if place mod 13 = 10 then Queen
      else if place mod 13 = 11 then King
      else Ace
    in
    Some (create_card valu sut) :: create_helper deck1 (place + 1)
  else to_deck deck1

let create : deck = create_helper [] 0

let rec shuffle_helper (deck1 : deck) (num : int) =
  if num < deck_size then
    match deck1 with
    | h :: t ->
        if Random.bool () then h :: shuffle_helper t (num + 1)
        else shuffle_helper t (num + 1) @ [ h ]
    | [] -> []
  else []

let rec shuffle_repeater (deck1 : deck) (num : int) (repetitions : int)
    =
  if num < repetitions then
    shuffle_repeater (shuffle_helper deck1 0) (num + 1) repetitions
  else deck1

(*IGNORE SEED*)
let shuffle (deck1 : deck) (seed : int) =
  Random.self_init ();
  to_deck (shuffle_repeater deck1 0 30)

let rec top_card (deck1 : deck) : card option =
  try List.hd (List.rev deck1) with _ -> None

let rec remove_top (deck1 : deck) =
  match deck1 with
  | [] -> []
  | [ h ] -> []
  | h :: t -> to_deck (h :: remove_top t)

let size (deck1 : deck) = List.length deck1
