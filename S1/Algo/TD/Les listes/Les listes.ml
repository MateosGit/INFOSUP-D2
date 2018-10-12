(*Exo 1.1*)
let rec sum = function
    [] -> 0
  | e::l -> e + sum l;;

(*Exo 1.2*)
let rec count l int = match l with
    [] -> 0
  | e::l when e=int -> 1+count l int
  | e::l -> count l int;;

(*Exo 1.3*)
let rec here e l= match l with
    [] -> false
  | a::_ when a =e -> true
  | _::l ->  here e l;;

(*Exo 1.4*)
let ieme rank list=
  if rank <= 0 then
    invalid_arg "ieme : the rank must be a natural different from 0"
  else
    let rec ieme_rec = function
        (_,[]) -> failwith "ieme: rank is too high"
      | (1, e::l) -> e
      | (i, e::l) -> ieme_rec (i-1, l)
    in ieme_rec (rank, list);;

(*Exo 1.5*)
let rec max = function
    []   -> invalid_arg "the list is empty"
  | e::[] -> e
  | e::l  -> let m = max l in
	     if m> e then
	       m
	     else
	       e;;

(*Exo Bonus*)
let is_empty l= l=[];;

let head = function
    [] -> failwith "head: the list is empty"
  | e::_ -> e;;

let _end = function
    [] -> failwith "_end : the list is empty..."
  | _::l -> l;;

let record = function
    []| _:: [] -> failwith "record: the list is empty..."
  | _::e::_ -> e;;

(*Exo 2.1*)
let arith_list n a r=
  let last = r*n +a-1 in
  let rec arith = function
      e when e> last -> []
    | e -> e::arith (e+r)
  in arith a;;

(*Exo 2.2*)
let sum_cumul l=
  let rec sum s = function
  [] -> []
    | e::l -> e+s::sum (e+s) l
  in sum 0 l;;

(*Exo 2.3*)
let rec append = function
    ([], l2) -> l2
  | (e::l, l2) -> e:: append (l, l2);;

(*Exo 3.1*)
let rec croissant = function
[] | [_] -> true
  | e:: f ::l when e<f -> croissant (f::l)
  | _ -> true;;

let rec growing = function
[]| [_] -> true
  | e:: f ::l -> e<=f && growing (f::l);;

(*Exo 3.2*)
let rec recherche e = function
[] -> false
  | s::q -> s=e||(s>e && recherche e q);;

(*Exo 3.3*)
let rec assoc k list = if k<=0 then
    invalid_arg "assoc: key <= 0"
  else
    let rec search = function
      | [] -> failwith "list is empty"
      | (int, str) :: list -> if int=k then
	  str
	else
	  if int>k then
	    failwith "assoc: key not found"
	  else
	    search list
    in search list;;

(*Exo 3.4*)
let rec suppr x = function
    [] -> []
  | e::l when e>x -> e::l
  | e::l when e=x -> l
  | e::l -> e::suppr x l;;

(*Exo 3.5*)
let rec insert x = function
    [] -> [x]
  | e::l when e<=x -> e:: x ::l
  | e::l -> e:: insert x l;;

(*Exo 3.6*)
let rec inverse = function
[] -> []
  | e::l -> (inverse l) @ [e];;

let inverse list=
  let rec inv list p = match list with
      [] -> p
    | e::list -> inv list (e::p)
  in inv list [];;

(*Exo 4.1*)
let rec equal = function

([], []) -> true
  |([], _) | (_, []) -> false
  | (e1::l1, e2::l2) -> e1 = e2 && equal (l1, l2);;


(*Exo 4.2*)
let rec merge = function
    ([], l)| (l, []) -> l
  | (e1:: l1, e2::l2) -> if e1=e2 then
      e2::merge (l1, l2)
    else if e1<e2 then
      e1:: merge (l1, (e2::l2))
    else
      e2:: merge ((e1::l1), l2);;
