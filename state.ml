(* Write in a function recursively that will keep on changing the init
   state based on how many players there are *)
type players =
  | Player
  | Computer of int

let int_of_player = function Player -> 0 | Computer x -> x

let player_of_int = function
  | 0 -> Player
  | x when x <= 7 -> Computer x
  | _ -> failwith "Error, maximum of 7 computer players"

let string_of_player = function
  | Player -> "You"
  | Computer x -> "Opponent" ^ string_of_int x

(* Game state *)
type state = {
  mutable users_hand : Deck.card list;
  mutable cpu_hands : Deck.card list array;
  mutable cards_on_table : Deck.card list;
  mutable deck_rem : Deck.deck;
  mutable turn : players;
  mutable user_money : int;
  mutable cpu_moneys : int array;
  mutable dealer : players;
  mutable current_bet : int;
}

let bankrupt index state =
  let players_mon =
    Array.append [| state.user_money |] state.cpu_moneys
  in
  let players_mon_8 = Array.make 8 0 in
  for i = 0 to Array.length players_mon - 1 do
    players_mon_8.(i) <- players_mon.(i)
  done;
  if players_mon_8.(index) = 0 && index < Array.length players_mon then
    true
  else false
