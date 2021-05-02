open State
open Compare

let pot = Array.make 8 (-2)

let money_back = Array.make 8 0

let add (mon : int) (player : State.players) =
  match player with
  | Computer x ->
      if mon >= 0 then
        if pot.(x) >= 0 then pot.(x) <- pot.(x) + mon
        else pot.(x) <- mon
      else ()
  | Player ->
      if mon >= 0 then
        if pot.(0) >= 0 then pot.(0) <- pot.(0) + mon
        else pot.(0) <- mon
      else ()

let reset () =
  for i = 0 to 7 do
    pot.(i) <- -2;
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

let rec piling ind acc =
  if ind >= 8 then acc
  else piling (ind + 1) (acc + if pot.(ind) >= 0 then pot.(ind) else 0)

let give_pot winner_list sum0 =
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

let rec remove_from_pot lst amt =
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

let to_list win_list =
  List.fold_right
    (fun a acc ->
      let num = to_player_num a in
      num :: acc)
    win_list []

let rec side_pot (win_list : win_record list) all_in (out : int list) =
  let rec num_all_in arr acc ind =
    if ind >= 8 then acc
    else if arr.(ind) = true then num_all_in arr (acc + 1) (ind + 1)
    else num_all_in arr acc (ind + 1)
  in
  if num_all_in all_in 0 0 = List.length out then
    give_pot (top_winners (subtract_win win_list out)) (piling 0 0)
  else
    let lowest_side =
      List.fold_right
        (fun a acc ->
          let p_num = to_player_num a in
          if
            (not (List.mem p_num out))
            && all_in.(p_num)
            && pot.(p_num) < acc
          then pot.(p_num)
          else acc)
        win_list 10000
    in
    let n = List.length (subtract (to_list win_list) out) in
    give_pot (top_winners (subtract_win win_list out)) (lowest_side * n);
    let side_cause =
      List.fold_right
        (fun a acc ->
          let p_num = to_player_num a in
          if pot.(p_num) = lowest_side then p_num else acc)
        win_list (-1)
    in
    remove_from_pot (subtract (to_list win_list) out) lowest_side;
    side_pot win_list all_in (side_cause :: out)

let to_winner (win_list : win_record list) (state : State.state) =
  let all_in = Array.make 8 false in
  for i = 0 to 7 do
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
