open State
open Compare

let unused = -2

let max_players = 8

let pot = Array.make max_players unused

let folded_pot = ref 0

let money_back = Array.make max_players 0

let over_total_money = 10000

let add_helper (mon : int) (num : int) =
  if mon >= 0 then
    if pot.(num) >= 0 then pot.(num) <- pot.(num) + mon
    else pot.(num) <- mon
  else (
    folded_pot := !folded_pot + pot.(num);
    pot.(num) <- unused)

let add (mon : int) (player : State.players) =
  match player with
  | Computer x -> add_helper mon x
  | Player -> add_helper mon 0

let reset () =
  for i = 0 to max_players - 1 do
    pot.(i) <- unused;
    money_back.(i) <- 0
  done

let to_player_num (win_rec : win_record) =
  match win_rec with
  | { player; rank; value } -> (
      match player with Player -> 0 | Computer x -> x)

let top_winners (win_list : win_record list) =
  let rank_max =
    List.fold_left
      (fun max a ->
        match a with
        | { player; rank; value } -> if rank > max then rank else max)
      0 win_list
  in
  let value_max =
    List.fold_left
      (fun max a ->
        match a with
        | { player; rank; value } ->
            if rank = rank_max && value > max then value else max)
      0 win_list
  in
  List.fold_left
    (fun acc a ->
      match a with
      | { player; rank; value } ->
          if rank = rank_max && value = value_max then
            (match player with Player -> 0 | Computer x -> x) :: acc
          else acc)
    [] win_list

let rec piling (ind : int) (acc : int) =
  let pile =
    if ind >= max_players then acc
    else piling (ind + 1) (acc + if pot.(ind) >= 0 then pot.(ind) else 0)
  in
  pile + !folded_pot

let give_pot (winner_list : int list) (sum0 : int) =
  if sum0 = 0 then ()
  else
    let num_winners = List.length winner_list in
    let sum =
      if num_winners > 1 && sum0 mod num_winners <> 0 then
        sum0 - (sum0 mod num_winners)
      else sum0
    in
    let rec splitter lst =
      match lst with
      | [] -> ()
      | [ h ] -> money_back.(h) <- money_back.(h) + (sum / num_winners)
      | h :: t ->
          money_back.(h) <- money_back.(h) + (sum / num_winners);
          splitter t
    in
    splitter winner_list

let rec remove_from_pot (lst : int list) (amt : int) =
  match lst with
  | [] -> ()
  | [ h ] -> pot.(h) <- pot.(h) - amt
  | h :: t ->
      pot.(h) <- pot.(h) - amt;
      remove_from_pot t amt

let subtract (lst1 : int list) (lst2 : int list) =
  List.filter (fun a -> if List.mem a lst2 then false else true) lst1

let subtract_win (lst1 : win_record list) (lst2 : int list) =
  List.filter
    (fun a ->
      let num = to_player_num a in
      if List.mem num lst2 then false else true)
    lst1

let to_list (win_list : win_record list) =
  List.fold_right
    (fun a acc ->
      let num = to_player_num a in
      num :: acc)
    win_list []

let lowest_side_helper
    (win_list : win_record list)
    (all_in : bool array)
    (out : int list) =
  List.fold_right
    (fun a acc ->
      let p_num = to_player_num a in
      if
        (not (List.mem p_num out))
        && all_in.(p_num)
        && pot.(p_num) < acc
      then pot.(p_num)
      else acc)
    win_list over_total_money

let rec side_pot
    (win_list : win_record list)
    (all_in : bool array)
    (out : int list) =
  let rec num_all_in arr acc ind =
    if ind >= max_players then acc
    else if arr.(ind) = true then num_all_in arr (acc + 1) (ind + 1)
    else num_all_in arr acc (ind + 1)
  in
  if num_all_in all_in 0 0 = List.length out then
    give_pot (top_winners (subtract_win win_list out)) (piling 0 0)
  else
    let lowest_side = lowest_side_helper win_list all_in out in
    let n = List.length (subtract (to_list win_list) out) in
    give_pot
      (top_winners (subtract_win win_list out))
      ((lowest_side * n)
      +
      if !folded_pot - lowest_side >= 0 then lowest_side
      else !folded_pot);
    let side_cause =
      List.fold_right
        (fun a acc ->
          let p_num = to_player_num a in
          if pot.(p_num) = lowest_side then p_num else acc)
        win_list (-1)
    in
    remove_from_pot (subtract (to_list win_list) out) lowest_side;
    folded_pot :=
      if !folded_pot - lowest_side >= 0 then !folded_pot - lowest_side
      else 0;
    side_pot win_list all_in (side_cause :: out)

let to_winner (win_list : win_record list) (state : State.state) =
  let all_in = Array.make max_players false in
  for i = 0 to max_players - 1 do
    if bankrupt i state = true then all_in.(i) <- true else ()
  done;
  let rec all_all_in_helper lst =
    match lst with
    | h :: t ->
        let num = to_player_num h in
        if all_in.(num) = false then false else all_all_in_helper t
    | [] -> true
  in
  let all_all_in = all_all_in_helper win_list in
  let rec any_all_in lst =
    match lst with
    | [] -> false
    | h :: t ->
        let num = to_player_num h in
        if all_in.(num) = true then true else any_all_in t
  in
  let side_needed =
    if not all_all_in then any_all_in win_list else false
  in
  if not side_needed then give_pot (top_winners win_list) (piling 0 0)
  else side_pot win_list all_in [];
  money_back

let print_pot () = string_of_int (piling 0 0 + !folded_pot)
