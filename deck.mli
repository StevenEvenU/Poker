(**The deck of cards in order. Able to be shuffled.*)

(** The abstract type of cards in order in a deck. *)
type t

(** The type of suits. *)
type suit = Spades | Hearts | Diamonds | Clubs

(** The type of values. *)
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