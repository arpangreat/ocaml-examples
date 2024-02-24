let () =
  print_endline "Hello, World!";

  let x = 5 in
  print_endline @@ string_of_int x
;;

let y = 4 |> succ in
print_endline @@ string_of_int y
;;

let z = match not true with true -> "true" | false -> "false" in
print_endline z

let x = 5
let y = 6

let n a =
  match a with
  | 5 -> print_endline @@ string_of_int x ^ " " ^ "printing from x"
  | 6 -> print_endline @@ string_of_int y ^ " " ^ "printing from y"
  | _ -> print_endline "not found"
;;

n 5;;

let b = match [ "swastik"; "acharyya" ] with [] -> "coder" | h :: _t -> h in
print_endline b

let fst3 t = match t with a, _, _ -> a;;

let a = fst3 (1, 2, 3) in
print_endline @@ string_of_int a

let rec sum = function [] -> 0 | h :: t -> h + sum t;;

let a = sum [ 1; 2; 3; 4; 5 ] in
print_endline @@ string_of_int a

let print1 x = print_endline x;;

print1 "string"

let avg a b = (a +. b) /. 2.0;;

print_endline @@ string_of_float (avg 5.0 6.0)

let get_val default o = match o with None -> default | Some x -> x

let rec list_max (lst : 'a list) : 'a option =
  match lst with [] -> None | h :: t -> Some (max h (list_max t |> get_val h))
;;

let x = list_max [ 1; 2; 3; 4; 5 ];;

print_endline @@ string_of_int (get_val 0 x)

let double x = x * 2
let twice f x = f (f x)
let quad x = twice double x;;

print_endline @@ string_of_int (quad 5)

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let t = Node (1, Node (2, Leaf, Leaf), Node (3, Leaf, Leaf))
let rec size = function Leaf -> 0 | Node (_, l, r) -> 1 + size l + size r
let rec sum = function Leaf -> 0 | Node (x, l, r) -> x + sum l + sum r;;

print_endline @@ string_of_int (size t) ^ " size of tree";;
print_endline @@ string_of_int (sum t) ^ " sum of tree"

(* let rec add = function [] -> [] | h :: t -> (h + 1) :: add t *)
let rec map f = function [] -> [] | h :: t -> f h :: map f t
let add' = map (fun x -> x + 1)

(* let rec combine init op = function *)
(*   | [] -> init *)
(*   | h :: t -> op h (combine init op t) *)
(* ;; *)

(* let add'' lst = combine 0 ( + ) lst *)

(* let rec fold_right f acc = function
     | [] -> acc
     | h :: t -> f h (fold_right f t acc)
   ;;

   let rec fold_left f acc = function
     | [] -> acc
     | h :: t ->
         let acc' = f acc h in
         fold_left f acc' t
   ;; *)

let rec print_lst = function
  | [] -> ()
  | h :: t ->
      print_endline @@ string_of_int h;
      print_lst t
;;

print_lst (add' [ 1; 2; 3; 4; 5 ])
(* print_lst (add'' [ 1; 2; 3; 4; 5 ]) *)

let even n = n mod 2 = 0

let rec evens = function
  | [] -> []
  | h :: t -> if even h then h :: evens t else evens t
;;

let lst1 = evens [ 1; 2; 3; 4 ];;

print_endline "Even numbers in a list";;
print_lst lst1

let odd n = n mod 2 <> 0

let rec odds = function
  | [] -> []
  | h :: t -> if odd h then h :: odds t else odds t
;;

let lst' = odds [ 1; 2; 3; 4 ];;

print_endline "Odd numbers in a list";;
print_lst lst'

let rec filter p = function
  | [] -> []
  | h :: t -> if p h then h :: filter p t else filter p t
;;

let evens' = filter even
let odds' = filter odd
let lst'' = [ 1; 2; 3; 4 ];;

print_lst @@ evens' lst'';;
print_lst @@ odds' lst''

let rec filter_aux p acc = function
  | [] -> List.rev acc
  | h :: t -> if p h then filter_aux p (h :: acc) t else filter_aux p acc t
;;

let filter p = filter_aux p []
let lst = filter even [ 1; 2; 3; 4 ];;

print_lst lst

let rec concat = function [] -> "" | h :: t -> h ^ concat t
let c = concat [ "S"; "w"; "a"; "s"; "t"; "i"; "k" ];;

print_newline @@ print_endline c
