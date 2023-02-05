
3110;;
2110;;
true;;
false;;
3110 > 2110;;
"big";;
"big" ^ "red";; (* "bigred" *)
2. *. 3.14;;
(3110 : int);;
(3110L : int64);;
(* (3110 : bool) *)
if "batman" > "superman" then "yay" else "boo";;

let x = 42;;
let (y : int) = 3110;;
x + y;;
let a = 0 in a;;
let b = 1 in 2 * b;;
let c = 3 in (let d = 4 in c + d);;
(* let e = 5 in (let e = 6 in e);; *)

(* anon. functions *)
fun x -> x + 1;;
(fun x -> x + 1) (3110);;
(fun x -> x + 1) (2 + 3);;
fun x y -> (x +. y) /. 2.;;
(fun x y -> (x +. y) /. 2.) 3110. 3111.;;
(fun x y -> x- y) (3 * 1) (3 - 1);;

let inc = fun x -> x + 1;;
inc 3110;;
let inc x = x + 1;;
inc 3110;;
let avg x y = fun x y -> (x +. y) /. 2.;;
let avg x y = (x +. y) /. 2.;; (* syntactic sugar*)
avg 0. 1.;;
(fun x -> x + 1) 2;;
let x = 2 in x + 1;; (* syntactic sugar*)
let f x y = x - y;;
f 3 2;;

(* requires: [n >= 0] *)
let rec fact n =
  if n = 0 then 1
  else n * fact (n - 1);;
fact 10;;

let add x y = x + y;;
add 2 3;;
add 2;; (* gives back a function *)
(add 2) 3;; (* gives 5 *)
let add2 = add 2;;
add2 10;;
(* multi-argument functions do not exist *)
(* fun x y -> e
   is syntactic sugar for
   fun x -> (fun y -> e) *)
(* let add x y = x + y
   is syntactic sugar for 
   let add = fun x ->
               fun y ->
                  x + y *)

let id x = x;;
id 5;;
( + ) 1 2;; (* int -> int -> int *)
( * ) 1 2;; (* int -> int -> int *)
( = );; (* 'a -> 'a -> bool *)
( = ) 1 2;; (* false *)
( < );; (* 'a -> 'a -> bool *)
max;; (* 'a -> 'a -> 'a *)
let ( <^> ) x y = max x y;;
1 <^> 2;;

(* application:
    let ( @@ ) f x = f x *)
succ 2 * 10;; (* gives 30 *)
succ (2 * 10);; (* gives 21 *)
succ @@ 2 * 10;; (* gives 21 *)
(* reverse application:
    let ( |> ) x f = f x *)
let square x = x * x;;
square (succ 5);; (* gives 36 *)
square (succ (square (succ 5)));; (* gives 1369*)
5 |> succ |> square |> succ |> square;; (* gives 1369*)

(* lsts - immutable *)
(* [] is called "nil",  :: is called "cons" *)
[];;
[1];; (* int list = [1] *)
[1;2];;
[true;false];;
[[1;2];[3;4]];; (* int list list *)
1 :: [2;3];; (* [1;2;3] *)
1 :: 2 :: 3 :: [];; (* [1;2;3] *)

(* records *)
(* order of fields is irrelevant *)
type student = {
  name : string;
  year : int;
}
let rbg : student = { (* type annotaion is not necessary, can be figured out with type inference *)
  name = "Ruth Bader";
  year = 1954;
};;
rbg.name;; (* gives "Ruth Bader" *)
rbg.year;; (* gives 1954 *)
rbg;;
{rbg with name = "Ginsberg"};; (* new record with different name *)
rbg;; (* line above did not go back and change the original record; records are immutable *)

(* tuples - unnamed components*)
(* order of fields is relevant *)
type time = int * int * string (* unlike records, the components are not named, like name or year *)
let t : time = (10,10,"am");;
type point = float * float
let p : point = (5.,3.5);;
p;;
fst;; (* func that takes in a pair, like a 'point', gives back the first component. *)
snd;;
fst p;;
snd p;;

(* pattern matching *)
match not true with | true -> "nope" | false -> "yep";;
let x =
  match not true with
  | true -> "nope"
  | false -> "yep";;
let y =
  match 42 with
  | foo -> foo;;
let z =
  match "foo" with
  | "bar" -> 0
  | _ -> 1;;
let a =
  match [1;2] with
  | [] -> "empty"
  | _ -> "not empty";;
let b =
  match ["taylor";"swift"] with
  | [] -> "folklore"
  | h :: t -> h;; (* t on the right side will give TypeError *)
let b =
  match ["taylor";"swift"] with
  | [] -> ["folklore"]
  | h :: t -> t;;
let fst3 t =
  match t with
  | (a,b,c) -> a;;
(* accessing to records by names is preferred to using tuples of more than 2 components *)
let name_with_year s =
  match s with
  | {name;year} -> name ^ " '" ^ string_of_int (year mod 100);;
name_with_year rbg;;
(* pattern matching with lists *)
let empty lst =
  match lst with
  | [] -> true
  | h :: t -> false;; (* better _ :: _ -> false or _ -> false *)
let rec sum lst = (* recursive *)
  match lst with
  | [] -> 0
  | h :: t -> h + sum t;;
sum [1;4;7];;
(* try #trace sum;; *)
let rec len lst =
  match lst with
  | [] -> 0
  | h :: t -> 1 + len t;;
len [1;4;7];;
(* exapmle usage:
   append [1;2;3] [4;5;6] is [1;2;3;4;5;6] *)
let rec append lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | h :: t -> h :: (append t lst2);; (* parantheses can be leaved out *)
append [1;2;3] [4;5;6];;
(* many of these list funcs, including append are built into the stdlib;
   built-in operator @ serves as append. *)
[1;2;3] @ [4;5;6];;

(* function keyword *)
let empty = function
  | [] -> true
  | _ -> false;;
let rec sum = function
  | [] -> 0
  | h :: t -> h + sum t;;
let rec length = function
  | [] -> 0
  | h :: t -> 1 + length t;;
(* for append, the funcion keyword cannot be used as it is above;
 * it means to immediately pattern match against the last argument;
 * which is not how append is implenented, it does against its first argument *)

(* operator :: (called "cons") prepends an element onto the head of a list
 * 'a -> 'a list -> 'a list, O(1) *)
(* operator @ (called "append") combines two lists
 * 'a list -> 'a list -> 'a list, O(length of first list) *)

(* data type patterns:
  []
  p1 :: p2
  [p1; p2]
  (p1, p2)
  {f1 = p1; f2 = p2}
  ...
*)

(* branches should be exhaustive *)
let empty lst =
  match lst with
  | [] -> true
  | _ -> false (* if this branch is left out, a warning is raised before/during compile time*)
;;
let rec sum lst =
  match lst with
  | h :: t -> h + sum t
(*| [x] -> x *) (* unused; will never be matched, because [x] equals x :: []
    and this is already covered in the fisrt branch. *)
  | [] -> 0
;;
let rec bad_sum lst =
  List.hd lst + bad_sum (List.tl lst)
(* no pattern matching occurs in this function. *)
;;
  (* what are these? *)
List.hd [1; 2; 3];; (* 1 *)
List.tl [1; 2; 3];; (* [2; 3] *)
List.hd [];; (* Exception: Failure "hd". *)
List.tl [];; (* Exception: Failure "tl". *)
;;

(* variants *)
type primary_color = (* type of which constructors do not cary data *)
  | Red
  | Green
  | Blue
let r = Red
;;
type point = float * float
type shape =
  | Circle of {center : point; radius : float}
  | Rectangle of {lower_left : point; upper_right : point}
(*| Rectangle of point * point *)
  | Point of point
let cl = Circle {center = (0., 0.); radius = 1.}
let rl = Rectangle {lower_left = (-1., -1.); upper_right = (1., 1.)}
let p1 = Point (31., 10.)

let center s = (* is the shape that passed in a Circle or a Rectangle? *)
  match s with
  | Circle {center; radius} -> center
  | Rectangle {lower_left; upper_right} ->
    let avg a b = (a +. b) /. 2. in
    let (x_ll, y_ll) = lower_left in
    let (x_ur, y_ur) = upper_right in
    (avg x_ll y_ur, avg y_ll y_ur)
  | Point (x, y) -> (x, y)
(* the function above is equivalent to: *)
let center_new s =
  match s with
  | Circle {center; radius} -> center
  | Rectangle {lower_left = (x_ll, y_ll); upper_right = (x_ur, y_ur)} ->
    let avg a b = (a +. b) /. 2. in
    (avg x_ll y_ur, avg y_ll y_ur)
(* deep pattern matching; patterns nested in paterns *)
  | Point p -> p (* constructor Point expects argument, either expressed as p or (x, y) *)
(* syntax: C e
   a constructor name followed by a patern, i.e.
   C p is itself a pattern.
   and C is a value and a pattern *)
;;

(* algebraic data types *)

(* records and tuples are "each-of" types, aka product types, like a cartesian product.
   variants, on the other hand, are "one-of" types, aka sum types. *)
type string_or_int =
  | String of string
  | Int of int
(* the two constructors come from two different sets *)
type blue_or_pink_int =
  | Blue of int
  | Pink of int
(* taking two copies of the set of all integers, and unioning them together,
   but keeping track of from which copy.
   this is called, in mathematics, a "tagged union" *)
;;
type ptype =
  | TNormal
  | TFire
  | TWater

  type peff =
  | ENormal (* without using modules, constructor names can shadow another *)
  | ENotVery
  | ESuper

let mult_of_eff = function
  | ENormal -> 1.
  | ENotVery -> 0.5
  | ESuper -> 2.0

let eff = function
  | (TFire, TFire) | (TWater, TWater) | (TFire, TWater) -> ENotVery
  | (TWater, TFire) -> ESuper
  | _ -> ENormal
(* to use without parentheses:
   let eff t1 t2 = match t1, t2 with
    | TFire, TFire | TWater, TWater | TFire, TWater -> ENotVery
    | ... *)

type mon = {
  name : string;
  hp : int;
  ptype : ptype;
}

let charmander = {
  name = "Charmander";
  hp = 39;
  ptype = TFire;
}
;;

(* recursive and parameterized variants *)

type intlist =
  | Nil
  | Cons of int * intlist

let rec length_intlist = function
  | Nil -> 0
  | Cons (_, t) -> 1 + length_intlist t
;;
Cons (1, Cons (2, Nil));;
Cons (1, Cons (2, Nil)) |> length_intlist;;

type stringlist =
  | Nil
  | Cons of int * stringlist

let rec length_stringlist = function
  | Nil -> 0
  | Cons (_, t) -> 1 + length_stringlist t
;;
(* 변수의 타입마다 일일이 함수 만들지 말고 paramaterized variant 사용ㄱㄱ *)

type 'a mylist =
  | Nil
  | Cons of 'a * 'a mylist

let rec length = function
  | Nil -> 0
  | Cons (_, t) -> 1 + length t
;;
Cons (1, Nil) |> length;;
Cons (true, Nil) |> length;;

(* the equivalent using brackets and operators: *)
type 'a mylist =
  | []
  | (::) of 'a * 'a mylist

let rec length = function
  | [] -> 0
  | _ :: t -> 1 + length t
;;
(* actual implementation of list in stdlib: *)
type 'a list = [] | (::) of 'a * 'a list
(* list is a type constructor parameterized on type variable 'a *)
(* [] and :: are constructors *)
;;

(* options *)
(* type 'a option = None | Some of 'a *)

(* think of a box that is either empty (None) or not (Some) *)
(* 'a is the thing inside the box, possibly None *)

None;; (* - : 'a option = None *)
Some 1;; (* - : int option = Some 1 *)
Some [1; 2; 3];; (* - : int list option = Some [1; 2; 3] *)

let get_val = function
  | None -> failwith "??\n"
  (* we don't know what 'a is
     so we cannot return some defualt value *)
  | Some x -> x
let get_val_new default = function
  | None -> default
  | Some x -> x
;;

let rec list_max (lst : 'a list) : 'a option =
  match lst with
  | [] -> None
(*|  h :: t -> Some (max h (list_max t)) *)
(* why does above line cause compilation error?
   the list_max taken recursively has type 'a option,
   while the h has type 'a. these two have different types so nope.
   in other words, t could either be Some or None.
   instead i should get the value out of the box -
   the list_max recursively taken. *)
  | h :: t -> ( (* begin (* instead of left parenthesis *) *)
    match list_max t with
    | None -> Some h
    | Some m -> Some (max h m)
  ) (* end (* instead of right parenthesis *) *) (* for nested pattern matching *)
(* pattern match against None, instead of NullPointerException *)
;;

(* exceptions *)
(* type exn *)

exception OhNo of string;;
OhNo "oops";;
raise (OhNo "oops");;

exception ABadThing;;
raise ABadThing;;
(* built-in extensible variant; add new constructors to it with exception *)
(* raise never returns a real value, so the return type is 'a *)
(* can treat it as any type desired *)
let x : int = raise ABadThing;;

(* some predefined exceptions: *)
(* exception Failure of string *)
   (* built-in function failwith : string -> 'a *)
(* exception InvalidArgument of string *)
   (* built-in function invalid_arg : string -> 'a *)
raise (Failure "my error message");;
failwith "my error message";;

let safe_div x y =
  try x / y with (* just like match with *)
  | Division_by_zero -> 0
;;
safe_div 4 0;;

(* if e ==> v then (try e ...) ==> v *)
(* if e raises an exception x then
   match x against the patterns,
   if none match, reraise x *)

(* binary trees! *)

(* compare: *)
(*
type 'a mylist =
  | Nil
  | Cons of 'a * 'a mylist
*)

type 'a tree =
  | Leaf (* an empty tree that contains nothing *)
  | Node of 'a * 'a tree * 'a tree (* contains value 'a, as well as two 'a trees *)
(* each node of 'a mylist has only one child while 'a tree has two *)
let t = 
  Node (1,
    Node (2, Leaf, Leaf),
    Node (3, Leaf, Leaf)
  )
(*
       1
    /    \
   2      3
  / \    / \
 l   l  l   l
*)

let rec size = function
  | Leaf -> 0
  | Node (_, l, r) -> 1 + size l + size r
(*
let rec sum = function
  | Nil -> 0
  | Con (h, t) = h + sum t
*)

let rec sum = function
  | Leaf -> 0
  | Node (v, l, r) -> v + sum l + sum r
;;
