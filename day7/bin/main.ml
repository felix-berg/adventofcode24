type operator = Mul | Add

let opt_or_else els opt = match opt with
  | Some v -> v
  | None -> els ()

let compute_operators (lhs: int) (rhs: int list): operator list option =
  let rec comp_op value xs: operator list option = match xs with 
    | [ x ] -> if value = x then (Some []) else None
    | x :: xss -> (
      let opt =
        if value mod x <> 0 then 
          None 
        else match comp_op (value / x) xss with 
          | Some ops -> Some (Mul :: ops)
          | _ -> None
      in match opt with 
        | Some ops -> Some ops
        | None -> match comp_op (value - x) xss with
          | Some ops -> Some (Add :: ops)
          | None -> None
    )
    | [] -> raise Exit
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
  ) |> String.concat " ")

let () = 
  let inputs = read_input () in
  let result = inputs |> List.filter_map(
    fun (y, xs) -> match compute_operators y xs with
      | Some _ -> Some y
      | None -> None
  ) |> List.fold_left (fun acc v -> acc + v) 0 in
  Printf.printf "Result: %d\n" result;
  print_newline ()
