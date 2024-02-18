type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let t = Node (1, Node (2, Leaf, Leaf), Node (3, Leaf, Leaf))
let rec size = function Leaf -> 0 | Node (_, l, r) -> 1 + size l + size r
let rec sum = function Leaf -> 0 | Node (x, l, r) -> x + sum l + sum r;;

print_endline @@ string_of_int (size t) ^ "size of tree";;
print_endline @@ string_of_int (sum t) ^ "sum of tree"
