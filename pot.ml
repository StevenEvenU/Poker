open Compare

type pot = int ref

let pot = ref 0

let add (mon : int) = pot := !pot + mon

let to_winner (win_rec_list : win_record list) =
  failwith "unimplemented"
