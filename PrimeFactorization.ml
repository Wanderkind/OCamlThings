open Scanf
open Printf
open List
;;
let rec pow a b =
  if b = 0 then 1
  else a*(pow a (b-1))
;;
let pf_array n = (* n > 1 *)
  let rec factor n k =
    let rec howmany n k =
      if n mod k > 0 then 0
      else howmany (n/k) k + 1 in
    let h = howmany n k in
    if h = 0 then factor n (k+1)
    else
      if n = pow k h then [(k, h)]
      else [(k, h)] @ factor (n/(pow k h)) (k+1) in
  factor n 2
;;
let a = read_int() |> pf_array in
for i=0 to length a - 1 do
  printf "%d %d\n" (fst (nth a i)) (snd (nth a i))
done
;;
