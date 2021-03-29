(**The deck of cards in order. Able to be shuffled.*)

(** The abstract type of cards in order in a deck. *)
type deck

(** The abstract type of a single card. *)
type card

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

(**Creates a shuffled deck of cards and returns it*)
val create : unit -> deck

(**Shuffles a deck (or smaller amount) of cards*)
val shuffle: deck -> unit

(**Removes the top card from the deck and gives the card*)
val remove: deck -> card