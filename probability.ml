open State
open Compare
open Deck

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
  let deck_left = deck_remover (List.map (fun a -> Some a) lst) in
  0.1
