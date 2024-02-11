let () =
  print_endline "Hello, World!";

  let x = 5 in
  print_endline @@ string_of_int x;

  let y = 4 |> succ in
  print_endline @@ string_of_int y;

  let z = match not true with true -> "true" | false -> "false" in
  print_endline z;

  (* let n = match 6 with x -> x | y -> y | _ -> 8 in
     print_endline (string_of_int n); *)
  let b = match [ "swastik"; "acharyya" ] with [] -> "coder" | h :: _t -> h in
  print_endline b;

  let fst3 t = match t with a, _, _ -> a in

  let a = fst3 (1, 2, 3) in
  print_endline @@ string_of_int a;

  let rec sum = function [] -> 0 | h :: t -> h + sum t in
  let a = sum [ 1; 2; 3; 4; 5 ] in
  print_endline @@ string_of_int a;

  let print1 x = print_endline x in

  print1 "string"
