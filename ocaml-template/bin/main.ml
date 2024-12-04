type 'a thing = Cons of 'a * 'a thing | Nil

let rec length l = match l with
  | Cons (_, xs) -> 1 + length xs
  | Nil -> 0

let reverse l = 
  let rec rev l acc = match l with
    | Cons (x, xs) -> rev xs (Cons (x, acc))
    | Nil -> acc 
  in
  rev l Nil

let rec map f l = match l with
  | Cons (x, xs) -> Cons (f x, map f xs)
  | Nil -> Nil

let foreach (f: 'a -> 'b) l = 
  let _ = map f l in 
  ()

let () = 
  let ints = Cons (2, Cons (3, Cons (4, Nil))) in 
  let strings = map (fun i -> string_of_int i) ints in
  Printf.printf "len: %d\n" (length strings);
  foreach print_string (reverse strings);
  print_string "\n"
