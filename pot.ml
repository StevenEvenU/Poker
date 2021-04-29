open State

let pot = Array.make 8 (-2)

let money_back = Array.make 8 0

let folded = Array.make 8 false

let add (mon : int) (player : State.players) =
  match player with
  | Computer x ->
      if mon >= 0 then
        if pot.(x) >= 0 then pot.(x) <- pot.(x) + mon
        else pot.(x) <- mon
      else folded.(x) <- true
  | Player ->
      if mon >= 0 then
        if pot.(0) >= 0 then pot.(0) <- pot.(0) + mon
        else pot.(0) <- mon
      else folded.(0) <- true

let reset =
  for i = 0 to 7 do
    pot.(i) <- -2;
    folded.(i) <- false
  done

let rec side_pot_need all_in ind =
  if ind >= 8 then false
  else if all_in.(ind) = true then true
  else side_pot_need all_in (ind + 1)

let rec piling ind acc =
  if ind >= 8 then acc
  else piling (ind + 1) (acc + if pot.(ind) >= 0 then pot.(ind) else 0)

let give_pot win_list sum0 =
  let num_winners = List.length win_list in
  let sum =
    if num_winners > 1 && sum0 mod num_winners <> 0 then
      sum0 - (sum0 mod num_winners)
    else sum0
  in
  let rec splitter lst =
    match lst with
    | [] -> failwith "not gonna happen buddy"
    | [ h ] -> money_back.(h) <- sum
    | h :: t ->
        money_back.(h) <- sum / num_winners;
        splitter t
  in
  splitter win_list

let rec subtract (lst1 : int list) (lst2 : int list) (acc : int list) =
  match lst1 with
  | [] -> acc
  | h :: t ->
      if List.mem h lst2 then subtract t lst2 acc
      else subtract t lst2 (h :: acc)

let yes_side_pot win_list state all_in =
  let rec side_amount_calc ind =
    if ind >= 8 then failwith "not happening"
    else if all_in.(ind) = true then pot.(ind)
    else side_amount_calc (ind + 1)
  in
  let side_amount = side_amount_calc 0 in
  (*how much from person in side pot*)
  let rec side_cause_calc ind =
    if ind >= 8 then failwith "not happening"
    else if all_in.(ind) = true then ind
    else side_cause_calc (ind + 1)
  in
  let side_cause = side_cause_calc 0 in
  (* who went all in*)
  let rec side_pot_pile ind acc =
    if ind >= 8 then acc
    else if pot.(ind) > 0 then
      side_pot_pile (ind + 1) (acc + side_amount)
    else side_pot_pile (ind + 1) acc
  in
  let side_pot_sum = side_pot_pile 0 0 in
  (* total amount in side pot*)
  let remove_side_quant =
    for i = 0 to 7 do
      if pot.(i) > 0 then pot.(i) <- pot.(i) - side_pot_sum else ()
    done
  in
  remove_side_quant;
  give_pot win_list side_pot_sum;
  give_pot (subtract win_list [ side_cause ] []) (piling 0 0)

let to_winner (win_list : int list) (state : State.state) =
  let all_in = Array.make 8 false in
  for i = 0 to 7 do
    match bankrupt i state with true -> all_in.(i) <- true | _ -> ()
  done;
  let side_needed = side_pot_need all_in 0 in
  if side_needed = false then give_pot win_list (piling 0 0)
  else yes_side_pot win_list state all_in;
  money_back
