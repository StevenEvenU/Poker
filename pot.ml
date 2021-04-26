open Compare

type pot = int ref

let pot = ref 0

let side_pot = ref 0

let add (mon : int) (all_in : bool) =
  if not all_in then pot := !pot + mon else failwith "unimplemented"

let reset = pot := 0

let to_winner (win_rec_list : win_record list) =
  failwith "unimplemented"
