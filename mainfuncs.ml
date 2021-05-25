open Deck
open Compare
open State
open Table
open Pot
open Betting

let str_of_val value =
  match value with
  | Two -> "2"
  | Three -> "3"
  | Four -> "4"
  | Five -> "5"
  | Six -> "6"
  | Seven -> "7"
  | Eight -> "8"
  | Nine -> "9"
  | Ten -> "10"
  | Jack -> "J"
  | Queen -> "Q"
  | King -> "K"
  | Ace -> "A"

let str_of_card (card : Deck.card) =
  str_of_val card.value ^ str_of_suit card.suit

let rec str_of_card_rec str cards =
  match cards with
  | [] -> str
  | [ h ] -> str ^ " " ^ str_of_card h
  | h :: t ->
      let s = str ^ " " ^ str_of_card h in
      str_of_card_rec s t

let str_of_cards cards = str_of_card_rec "" cards

