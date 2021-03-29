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

type deck = card option array

let createCard valu sut = { suit = sut; value = valu }

let create_empty_helper : deck = Array.make 52 None

let create : deck =
  let deck1 = create_empty_helper in
  for y = 0 to 51 do
    let sut =
      if y < 13 then Hearts
      else if y < 26 then Diamonds
      else if y < 39 then Spades
      else Clubs
    in
    let valu =
      if y mod 13 = 0 then Two
      else if y mod 13 = 1 then Three
      else if y mod 13 = 2 then Four
      else if y mod 13 = 3 then Five
      else if y mod 13 = 4 then Six
      else if y mod 13 = 5 then Seven
      else if y mod 13 = 6 then Eight
      else if y mod 13 = 7 then Nine
      else if y mod 13 = 8 then Ten
      else if y mod 13 = 9 then Jack
      else if y mod 13 = 10 then Queen
      else if y mod 13 = 11 then King
      else Ace
    in
    deck1.(y) <- Some (createCard valu sut)
  done;
  deck1

let shuffle = failwith "not"

let remove = failwith "not"

let size = failwith "not"
