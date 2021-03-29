type suit = Spades | Hearts | Diamonds | Clubs

type value = Two 
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

type card ={suit : suit; value : value} 

type deck = card option array 

let createCard valu sut = {suit = sut; value = valu}  

let create : deck= 
  let card1 = 
    createCard Jack Hearts 
  in 
  Array.make 52 (Some card1)

let shuffle = failwith "not"

let remove = failwith "not"

let size = failwith "not"

let deck_to_string = failwith "not"