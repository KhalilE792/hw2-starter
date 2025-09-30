let abs x =
  if x >= 0 then x
  else (-x)

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup (tup : 'a * 'b) = (snd tup, fst tup)
let rev_triple (tup : 'a * 'b * 'c) = (thd3 tup, snd3 tup, fst3 tup)

let is_odd x = if (x % 2) = 1 then true else false

let is_older (date1: int * int * int) (date2: int * int * int) = 
  if fst3 date1 < fst3 date2 then true else
  if fst3 date1 > fst3 date2 then false else
    if snd3 date1 < snd3 date2 then true else
    if snd3 date1 > snd3 date2 then false else
      if thd3 date1 < thd3 date2 then true else false

let to_us_format (date1: int * int * int) = (snd3 date1, thd3 date1, fst3 date1)
(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec pow x p = failwith "unimplemented"

let rec fac n = failwith "unimplemented"

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get_nth ((idx:int), (lst: 'a list)) = failwith "unimplemented"

let larger lst1 lst2 = failwith "unimplemented"

let sum lst1 lst2 = failwith "unimplemented"
