open Deck

(* Types of user action *)
type action = 
| Check
| Call
| Raise
| Fold

(* Checks whether another player has raised *)
let is_raised state =
  failwith "Not implemented yet"

(* Get the user's next action based on the current state *)
let rec get_action state = 
  if (is_raised state)
  then (print_string "Do you wish to Call, Raise, or Fold?";
    match read_line () with
    | "Call" -> Call
    | "Raise" -> Raise
    | "Fold" -> Fold
    | _ -> (print_string "Not Valid. Try again"; get_action state)
  )
  else (print_string "Do you wish to Check, Raise, or Fold?";
    match read_line () with
    | "Check" -> Check
    | "Raise" -> Raise
    | "Fold" -> Fold
    | _ -> (print_string "Not Valid. Try again"; get_action state)
  )

  let string_of_suit suit : Deck.suit = function
    | Spades -> "♠"
    | Hearts -> "♥"
    | Diamonds -> "♦"
    | Clubs -> "♣"
    | _ -> failwith "Error! Unrecognised Suit"

  let string_of_value value : Deck.value = function
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
    | _ -> failwith "Error! Unrecognized Value"

  let string_of_card card : Deck.card =
    string_of_value card.value ^ string_of_suit card.suit

  let rec string_of_cards str cards : Deck.card list = function
    | [] -> str
    | h::[] -> str ^" "^ string_of_card h
    | h::t -> str ^" "^ string_of_card h ^" "^ string_of_cards t
    | _ -> failwith "Error unknown card list structure while converting to string"


  let output_state state = 
    state.cards_on_table

  let main = 
    (* Get Number of players *)
    print_string "Welcome to Poker! How many people do you want to play against?"
    let num_players = read_int ()

    (* Get the initial state based on number of players *)
    (* let state = construct num_players *)
    