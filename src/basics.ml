let abs x =
  if x >= 0 then x
  else (-x)

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup (tup : 'a * 'b) = (snd tup, fst tup);;
let rev_triple (tup : 'a * 'b * 'c) = (thd3 tup, snd3 tup, fst3 tup);;

let is_odd x = if (x % 2) = 1 then true else false;;

let is_older (date1: int * int * int) (date2: int * int * int) = 
  if fst3 date1 < fst3 date2 then true else
    if fst3 date1 > fst3 date2 then false else
      if snd3 date1 < snd3 date2 then true else
        if snd3 date1 > snd3 date2 then false else
          if thd3 date1 < thd3 date2 then true else false
;;

let to_us_format (date1: int * int * int) = (snd3 date1, thd3 date1, fst3 date1);;
(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec pow x p = if p = 0 then 1 else (x * pow x (p - 1));;

let rec fac n = if n = 1 then 1 else (n * fac (n - 1));;

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get_nth ((idx:int), (lst: 'a list)) = if idx = 0 then List.hd lst else get_nth ((idx - 1), List.tl lst);;

(*helper function*)
let rec list_length (lst: 'a list) = if lst = [] then 0 else (1 + list.length (List.tl lst));;

let larger lst1 lst2 = 
  if list_length lst1 = list_length lst2 then [] else
    if list_length lst1 > list_length lst2 then lst1 else lst2
;;

(*helper function*)
let rec single_sum (lst: int list) = if lst = [] then 0 else (List.hd lst + single_sum (List.tl lst));;

let sum lst1 lst2 = single_sum lst1 + single_sum lst2;;
