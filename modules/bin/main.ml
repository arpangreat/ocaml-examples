[@@@ocaml.warning "-27-32-33-34"]

module MyList = struct
  type 'a mylist = [] | ( :: ) of 'a * 'a mylist

  (* let rec map f = function [] -> () | h :: t -> f h :: map f t *)
  let[@tail_mod_cons] rec map f = function
    | [] -> []
    | [ a1 ] ->
        let r1 = f a1 in
        [ r1 ]
    | a1 :: a2 :: l ->
        let r1 = f a1 in
        let r2 = f a2 in
        r1 :: r2 :: map f l
end

module Tree = struct
  type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

  let rec map f = function
    | Leaf -> Leaf
    | Node (v, l, r) -> Node (f v, map f l, map f r)
end

(* let lst = MyList.map succ (Cons (1, Nil)) *)
(* let lst'' = MyList.Cons (lst, Nil) *)

(* let lst' = MyList.Cons;; *)

(* let () = Fmt.(pf stdout "%a" (list int) lst) *)
(* MyList.print_lst lst'' *)

let rec print_lst = function
  | MyList.[] -> ()
  | MyList.(h :: t) ->
      print_endline @@ string_of_int h;
      print_lst t

(* let tree = Tree.Node (1, Leaf, Leaf) *)
let list = MyList.[ 1; 2; 3 ] |> print_lst

module type StackSig = sig
  type 'a stack

  val empty : 'a stack
  val push : 'a -> 'a stack -> 'a stack
  val peek : 'a stack -> 'a
  val pop : 'a stack -> 'a stack
end

module MyStack : StackSig = struct
  type 'a stack = Empty | Entry of 'a * 'a stack

  let empty = Empty
  let push x s = Entry (x, s)
  let peek = function Empty -> failwith "Empty" | Entry (x, _) -> x
  let pop = function Empty -> failwith "Empty" | Entry (_, s) -> s
end

module ListStack : StackSig = struct
  type 'a stack = 'a list

  let empty = []
  let push x s = x :: s
  let peek = function [] -> failwith "Empty" | x :: _ -> x
  let pop = function [] -> failwith "Empty" | _ :: s -> s
end

let rec print_lst' = function
  | ListStack.[] -> ()
  | ListStack.(h :: t) ->
      print_endline @@ string_of_int h;
      print_lst' t

let w =
  let open ListStack in
  empty |> push 43 |> peek
;;

(* print_lst' w;; *)
print_int w |> print_newline

module type Queue = sig
  type 'a queue

  val empty : 'a queue
  val enqueue : 'a -> 'a queue -> 'a queue
  val peek : 'a queue -> 'a option
  val dequeue : 'a queue -> 'a queue option
end

module ListQueue : Queue = struct
  type 'a queue = 'a list

  let empty = []
  let enqueue x q = q @ [ x ]
  let peek = function [] -> None | x :: _ -> Some x
  let dequeue = function [] -> None | _ :: q -> Some q
end

module TwoQueue = struct
  type 'a queue = { front : 'a list; back : 'a list }

  let empty = { front = []; back = [] }

  let peek = function
    | { front = []; _ } -> None
    | { front = x :: _; _ } -> Some x

  let dequeue x = function
    | { front = []; _ } -> None
    | { front = _ :: []; back } -> Some { front = List.rev back; back = [] }
    | { front = _ :: t; back } -> Some { front = t; back }

  let enqueue x = function
    | { front = []; _ } -> { front = [ x ]; back = [] }
    | q -> { q with back = x :: q.back }
end

let ( >>| ) opt f = match opt with None -> None | Some x -> Some (f x)

(* let q : int list option =
     ListQueue.(empty |> enqueue 42 |> dequeue >>| enqueue 43)

   let ( >>= ) opt f = match opt with None -> None | Some x -> f x

   let q : int list option =
     let open ListQueue in
     empty |> enqueue 42 |> dequeue >>| enqueue 43 >>= dequeue *)

module type Fact = sig
  val fact : int -> int
end

module RecursiveFact : Fact = struct
  let rec fact n = if n = 0 then 1 else n * fact (n - 1)
end

module TailRecursiveFact : Fact = struct
  let rec fact_aux n acc = if n = 0 then acc else fact_aux (n - 1) (n * acc)
  let fact n = fact_aux n 1
end

let f = TailRecursiveFact.fact 10
let x = TailRecursiveFact.fact 10;;

print_newline @@ print_endline @@ string_of_int f;;
print_newline @@ print_endline @@ string_of_int x
