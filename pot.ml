open Compare
open State

(*let valu_0 = ref 0

  let max_0 = ref 1000

  let valu_1 = ref 0

  let max_1 = ref 0

  let valu_2 = ref 0

  let max_2 = ref 0

  let valu_3 = ref 0

  let max_3 = ref 0

  let valu_4 = ref 0

  let max_4 = ref 0

  let valu_5 = ref 0

  let max_5 = ref 0

  let valu_6 = ref 0

  let max_6 = ref 0

  let valu_7 = ref 0

  let max_7 = ref 0

  (*tuple of pot value and maximum add size*) let pot = (*Array.make 8
  (ref 0, ref 0)*) [| (valu_0, max_0); (valu_1, max_1); (valu_2, max_2);
  (valu_3, max_3); (valu_4, max_4); (valu_5, max_5); (valu_6, max_6);
  (valu_7, max_7); |]

  let minimum (state : State.state) = Array.fold_right (fun x min -> if
  x < min then x else min) state.cpu_moneys 1000

  let make_side (mon : int) = if !max_1 = 0 then max_1 := mon else if
  !max_2 = 0 then ( max_2 := mon; valu_2 := mon) else if !max_3 = 0 then
  ( max_3 := mon; valu_3 := mon) else if !max_4 = 0 then ( max_4 := mon;
  valu_4 := mon) else if !max_5 = 0 then ( max_5 := mon; valu_5 := mon)
  else if !max_6 = 0 then ( max_6 := mon; valu_6 := mon) else if !max_7
  = 0 then ( max_7 := mon; valu_7 := mon)

  let add_to_side mon = failwith "nope"

  let add (state : State.state) (mon : int) (all_in : bool) = let min =
  minimum state in (* if all_in then make_side mon else if !max_1 = 0
  then valu_0 := !valu_0 + mon else add_to_side mon*)

  (*if not all_in then pot.(0) := !pot + mon else let looping = 1 in
  while looping < 9 do pot.(looping - 1) <- (ref mon, ref mon) done*)

  let reset = for i = 0 to 7 do pot.(i) <- (ref 0, ref 0) done

  let to_winner (win_rec_list : win_record list) = failwith
  "unimplemented"*)

(** If it is -2 at the end of a hand then that player is not in the game*)
let pot = Array.make 8 (-2)

let folded = Array.make 8 false

let add (mon : int) (player : players) =
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

(*if mon >= 0 then pot.(0) <- pot.(0) + mon else folded.(0) <- true*)

let reset =
  for i = 0 to 7 do
    pot.(i) <- -2;
    folded.(i) <- false
  done

let waa i = true

let to_winner (win_rec_list : win_record list) (state : State.state) =
  let all_in = Array.make 8 false in
  for i = 0 to 7 do
    match bankrupt i state with true -> all_in.(i) <- true | _ -> ()
  done;
  let side_needed = ref false in
  for i = 0 to 7 do
    if all_in.(i) = true then side_needed := true else ()
  done;
  if !side_needed = false then
    let money_back = Array.make 8 0 in
    let rec money_list lst =
      match lst with
      | [] -> ()
      | [ h ] -> (
          match h.player with
          | Player -> money_back.(0) <- 1
          | Computer x -> money_back.(x) <- 1)
      | h :: t ->
          (match h.player with
          | Player -> money_back.(0) <- 1
          | Computer x -> money_back.(x) <- 1);
          money_list t
    in
    money_list win_rec_list
  else ()
