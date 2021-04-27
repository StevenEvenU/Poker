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

let pot (state : State.state) =
  Array.make (Array.length state.cpu_hands + 1) 0

let add (state : State.state) (mon : int) (player : players) =
  failwith "not yet"

let to_winner (win_rec_list : win_record list) =
  failwith "unimplemented"
