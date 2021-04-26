open Compare

type pot = int ref

let pot = ref 0

(*tuple of pot value and maximum add size*)
let side_pot = Array.make 8 (ref 0, ref 0)

let add (mon : int) (all_in : bool) =
  if not all_in then pot := !pot + mon else failwith "unimplemented"

let reset = pot := 0

let to_winner (win_rec_list : win_record list) =
  failwith "unimplemented"
