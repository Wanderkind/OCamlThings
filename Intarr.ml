open Scanf
;;
let rec intarr = function 1 -> [scanf "%d\n" @@ fun n -> n] | x -> scanf "%d " @@ fun n -> (n :: intarr @@ x - 1)
;;
