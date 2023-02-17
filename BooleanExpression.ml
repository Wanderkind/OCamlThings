type 'a expr =
| True
| False
| And  of 'a expr * 'a expr
| Or   of 'a expr * 'a expr
| Not  of 'a expr
| Base of 'a
;;
let rec eval eval_base expr =
  let eval' x = eval eval_base x in
  match expr with
  | True       -> true
  | False      -> false
  | Base base  -> eval_base base
  | And (x, y) -> eval' x && eval' y
  | Or  (x, y) -> eval' x || eval' y
  | Not x -> not (eval' x)
;;
