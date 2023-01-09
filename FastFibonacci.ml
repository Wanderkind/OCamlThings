open Printf
open List
open Int64
;;
let ( + ) a b = rem (add a b) 1000000007L
let ( * ) a b = rem (mul a b) 1000000007L
;;
let rec bin n =
  if n < 2 then [n mod 2 = 1]
  else bin (n/2) @ [n mod 2 = 1]
;;
let cut = function
  | [] -> [] 
  | _ :: xs -> xs
;;
let mm a b =
  let ((p, q), (r, s)) = a in
  let ((t, u), (v, w)) = b in
  ((p*t+q*v, p*u+q*w), (r*t+s*v, r*u+s*w))
;;
let f n =
  let rec ff p q r =
    if length r = 1 then
      if nth r 0 then (mm p q, q)
      else (p, q)
    else
      let (pp, qq) = ff p q (cut r) in
      let qqq = mm qq qq in
      if nth r 0 then (mm pp qqq, qqq)
      else (pp, qqq) in
  ff ((1L, 0L), (0L, 1L)) ((1L, 1L), (1L, 0L)) (bin n) |> fst |> fst |> snd
;;
let n = read_int() in printf "%Ld" (f n)
;;
