type operator = Mul | Add | Concat

let opt_or_else els opt = match opt with
  | Some v -> v
  | None -> els ()

let opt_flat_map (f: 'a -> 'b option) (opt: 'a option) =
  opt |> Option.map f |> Option.join

let opt_or_else_opt (els: unit -> 'a option) (opt: 'a option) : 'a option = 
  match opt with
    | Some v -> Some v
    | None -> els ()

let ten_to_the p = 
  int_of_string ("1" ^ (Seq.init p (fun _ -> '0') |> String.of_seq))

let unconcat v ~suffix : int option = 
  let digits = String.length (string_of_int suffix) in
  let tentolen = ten_to_the digits in
  let u = v - suffix in
  if u mod tentolen <> 0 then 
    None 
  else 
    Some (u / tentolen)

let compute_operators (lhs: int) (rhs: int list): operator list option =
  let rec comp_op value xs: operator list option = 
    let try_mul x xss : operator list option = 
      if value mod x <> 0 then None
      else 
        comp_op (value / x) xss 
          |> Option.map (fun ops -> Mul :: ops)
    and try_add x xss : operator list option =
      comp_op (value - x) xss 
        |> Option.map (fun ops -> Add :: ops)
    and try_concat x xss : operator list option =
      unconcat value ~suffix:x |> opt_flat_map (
        fun new_val -> comp_op new_val xss
      ) |> Option.map(fun ops -> Concat :: ops)
    in
    match xs with 
      | [ x ] -> if value = x then (Some []) else None
      | x :: xss ->
        try_mul x xss 
          |> opt_or_else_opt (fun _ -> try_add x xss)
          |> opt_or_else_opt (fun _ -> try_concat x xss)
      | [] -> assert false
  in
  comp_op lhs (List.rev rhs)

let read_line_opt () = try Some (read_line ()) with End_of_file -> None

type input = (int * (int list)) list
let rec read_lines () = 
  match read_line_opt () with
    | Some line -> line :: read_lines()
    | None -> []

let read_input (): input = 
  let lines = read_lines () in
  lines |> List.map(fun line -> 
    match String.split_on_char ':' line with
      | [ ystr; xsstr ] ->
        let tail = String.sub xsstr 1 (String.length xsstr - 1) in
        let xs = String.split_on_char ' ' tail |> List.map (int_of_string) in
        (int_of_string ystr, xs)
      | _ -> raise (Failure ("invalid line: " ^ line))
  )

let weave (xs: 'a list) (ys: 'b list) = 
  let rec w1 xs ys = match xs with 
    | x :: xss -> (Either.Left x) :: (w2 xss ys)
    | [] -> ys |> List.map (fun y -> Either.Right y)
  and w2 xs ys = match ys with
    | y :: yss -> (Either.Right y) :: (w1 xs yss)
    | [] -> xs |> List.map (fun x -> Either.Left x)
  in 
  w1 xs ys

let get_result_string y xs ops =
  (string_of_int y) ^ " = " ^
  (weave xs ops
  |> List.map(fun elm ->
      match elm with
        | Either.Left x -> string_of_int x
        | Either.Right op -> match op with
          | Mul -> "*"
          | Add -> "+"
          | Concat -> "||"
  ) |> String.concat " ")

let part_two inputs = 
  let result = inputs |> List.filter_map(
    fun (y, xs) -> match compute_operators y xs with
      | Some ops -> 
        print_endline (get_result_string y xs ops);
        Some y
      | None -> None
  ) |> List.fold_left (fun acc v -> acc + v) 0 in
  Printf.printf "Result: %d\n" result;
  print_newline ()

let () = 
  let inputs = read_input () in
  part_two inputs;
  print_endline (string_of_int (unconcat 15002123010 ~suffix:2123010 |> opt_or_else (fun _ -> 0)))
