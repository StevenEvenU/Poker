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

(* let rec side_pot_need all_in ind = if ind >= 8 then false else if
   all_in.(ind) = true then true else side_pot_need all_in (ind + 1)

   let rec piling ind acc = if ind >= 8 then acc else piling (ind + 1)
   (acc + if pot.(ind) >= 0 then pot.(ind) else 0)

   let give_pot win_list sum0 = let num_winners = List.length win_list
   in let sum = if num_winners > 1 && sum0 mod num_winners <> 0 then
   sum0 - (sum0 mod num_winners) else sum0 in let rec splitter lst =
   match lst with | [] -> () | [ h ] -> money_back.(h) <- money_back.(h)
   + (sum / num_winners) | h :: t -> money_back.(h) <- money_back.(h) +
   (sum / num_winners); splitter t in splitter win_list

   let rec subtract (lst1 : int list) (lst2 : int list) (acc : int list)
   = match lst1 with | [] -> acc | h :: t -> if List.mem h lst2 then
   subtract t lst2 acc else subtract t lst2 (h :: acc)

   let top_winners (win_list : win_record list) = let rank_max =
   List.fold_left (fun max a -> match a with | { player; rank; value }
   -> if rank > max then rank else max) 0 win_list in let value_max =
   List.fold_left (fun max a -> match a with | { player; rank; value }
   -> if rank = rank_max && value > max then value else max) 0 win_list
   in List.fold_left (fun acc a -> match a with | { player; rank; value
   } -> if rank = rank_max && value = value_max then (match player with
   Player -> 0 | Computer x -> x) :: acc else acc) [] win_list

   let second_place win_list = let rank_max = List.fold_left (fun max a
   -> match a with | { player; rank; value } -> if rank > max then rank
   else max) 0 win_list in let value_max = List.fold_left (fun max a ->
   match a with | { player; rank; value } -> if rank = rank_max && value
   > max then value else max) 0 win_list in let second = List.fold_left
   (fun sec a -> match a with | { player; rank; value } -> if value >
   sec && value < value_max then value else sec) 0 win_list in let
   sec_lst = List.fold_left (fun acc a -> match a with | { player; rank;
   value } -> if value = second && rank = rank_max then (match player
   with Player -> 0 | Computer x -> x) :: acc else acc) [] win_list in
   if sec_lst <> [] then sec_lst else let rank_sec = List.fold_left (fun
   max a -> match a with | { player; rank; value } -> if rank > max &&
   rank < rank_max then rank else max) 0 win_list in List.fold_left (fun
   acc a -> match a with | { player; rank; value } -> if rank = rank_sec
   then (match player with Player -> 0 | Computer x -> x) :: acc else
   acc) [] win_list

   let remainder win_list side_cause = let sub = subtract (top_winners
   win_list) [ side_cause ] [] in if sub <> [] then sub else
   second_place win_list

   let one_side_pot win_list state all_in = let rec side_amount_calc ind
   = if ind >= 8 then failwith "not happening 3" else if all_in.(ind) =
   true then pot.(ind) else side_amount_calc (ind + 1) in let
   side_amount = side_amount_calc 0 in (*how much from person in side
   pot*) let rec side_cause_calc ind = if ind >= 8 then failwith "not
   happening 2" else if all_in.(ind) = true then ind else
   side_cause_calc (ind + 1) in let side_cause = side_cause_calc 0 in (*
   who went all in*) let rec side_pot_pile ind acc = if ind >= 8 then
   acc else if pot.(ind) > 0 then side_pot_pile (ind + 1) (acc +
   side_amount) else side_pot_pile (ind + 1) acc in let side_pot_sum =
   side_pot_pile 0 0 in (* total amount in side pot*) for i = 0 to 7 do
   if pot.(i) > 0 then pot.(i) <- pot.(i) - side_amount else () done;
   give_pot (top_winners win_list) side_pot_sum; give_pot (remainder
   win_list side_cause) (piling 0 0)

   let remove_causer (win_list : win_record list) (side_cause : int) =
   List.filter (fun a -> match a with | { player; rank; value } -> if
   (match player with Player -> 0 | Computer x -> x) = side_cause then
   false else true) win_list

   let rec n_side_pot win_list state all_in n = let rec side_amount_calc
   ind acc = if ind >= 8 then acc else if all_in.(ind) = true &&
   pot.(ind) < acc then side_amount_calc (ind + 1) pot.(ind) else
   side_amount_calc (ind + 1) acc in let side_amount = side_amount_calc
   0 10000 in (*how much from person in side pot*) let rec
   side_cause_calc ind = if ind >= 8 then failwith ("not happening " ^
   string_of_int side_amount) else if pot.(ind) = side_amount then ind
   else side_cause_calc (ind + 1) in let side_cause = side_cause_calc 0
   in (* who went all in*) let rec side_pot_pile ind acc = if ind >= 8
   then acc else if pot.(ind) > 0 then side_pot_pile (ind + 1) (acc +
   side_amount) else side_pot_pile (ind + 1) acc in let side_pot_sum =
   side_pot_pile 0 0 in (* total amount in side pot*) for i = 0 to 7 do
   if pot.(i) > 0 then pot.(i) <- pot.(i) - side_amount else () done;
   give_pot (top_winners win_list) side_pot_sum; for i = 0 to 7 do if
   all_in.(i) = true && pot.(i) = 0 then all_in.(i) <- false else ()
   done; if n <= 1 || not (side_pot_need all_in 0) then () else
   n_side_pot (remove_causer win_list side_cause) state all_in (n - 1);
   give_pot (remainder win_list side_cause) (piling 0 0)

   let yes_side_pot win_list state all_in = let rec num_side_pot_calc
   ind acc = if ind >= 8 then acc else if all_in.(ind) = true then
   num_side_pot_calc (ind + 1) (acc + 1) else num_side_pot_calc (ind +
   1) acc in let num_side_pot = num_side_pot_calc 0 0 in if num_side_pot
   = 1 then one_side_pot win_list state all_in else n_side_pot win_list
   state all_in num_side_pot

   let to_winner (win_list : win_record list) (state : State.state) =
   let all_in = Array.make 8 false in for i = 0 to 7 do if bankrupt i
   state = true then all_in.(i) <- true else () done; let side_needed =
   side_pot_need all_in 0 in let rec aaa lst acc = match lst with | []
   -> acc | h :: t -> ( match h with | { player; rank; value } -> let x
   = match player with Player -> 0 | Computer y -> y in if all_in.(x)
   then aaa t (acc + 1) else aaa t acc) in let side_needed_1 = if aaa
   win_list 0 = List.length win_list then false else true in if
   side_needed || side_needed_1 = false then give_pot (top_winners
   win_list) (piling 0 0) else yes_side_pot win_list state all_in;
   money_back *)

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

let subtract lst1 lst2 =
  List.filter (fun a -> if List.mem a lst2 then false else true) lst1

let rec side_pot win_list all_in out =
  let rec num_all_in arr acc ind =
    if ind >= 8 then acc
    else if arr.(ind) = true then num_all_in arr (acc + 1) (ind + 1)
    else num_all_in arr acc (ind + 1)
  in
  if num_all_in all_in 0 0 = List.length out then
    give_pot (subtract (top_winners win_list) out) (piling 0 0)
  else
    let lowest_side =
      List.fold_right
        (fun a acc ->
          match a with
          | { player; rank; value } ->
              let p =
                match player with Player -> 0 | Computer x -> x
              in
              if (not (List.mem p out)) && all_in.(p) && pot.(p) < acc
              then pot.(p)
              else acc)
        win_list 10000
    in
    give_pot (subtract (top_winners win_list) out) lowest_side

let to_winner (win_list : win_record list) (state : State.state) =
  let all_in = Array.make 8 false in
  for i = 0 to 7 do
    if bankrupt i state = true then all_in.(i) <- true else ()
  done;
  let rec all_all_in_helper lst =
    match lst with
    | h :: t -> (
        match h with
        | { player; rank; value } ->
            let y = match player with Player -> 0 | Computer x -> x in
            if all_in.(y) = false then false else all_all_in_helper t)
    | [] -> true
  in
  let all_all_in = all_all_in_helper win_list in
  let rec any_all_in lst =
    match lst with
    | [] -> false
    | h :: t -> (
        match h with
        | { player; rank; value } ->
            let y = match player with Player -> 0 | Computer x -> x in
            if all_in.(y) = true then true else any_all_in t)
  in
  let side_needed =
    if not all_all_in then any_all_in win_list else false
  in
  if not side_needed then give_pot (top_winners win_list) (piling 0 0)
  else side_pot win_list all_in [];
  reset ();
  money_back
