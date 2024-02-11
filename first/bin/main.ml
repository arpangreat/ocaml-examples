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

n 6;;

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
