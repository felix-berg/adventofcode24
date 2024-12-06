module Ints = Set.Make(Int)
module IntMap = Map.Make(Int)

type input = {
  rules: Ints.t IntMap.t;
  updates: (int Seq.t) Seq.t;
}

let read_line_opt (): string option = 
  try Some (read_line ())
  with Stdlib.End_of_file -> None

let rec read_lines_until_newline (): string list = 
  match read_line_opt () with
    | Some "" | None -> []
    | Some line -> line :: read_lines_until_newline ()

let read_input (): input = 
  let parse_rule line: (int * int) = 
    let ints = line |> String.split_on_char '|' |> List.map int_of_string in
    match ints with
      | [k; v] -> (k, v)
      | _ -> raise Stdlib.Exit
  and parse_update line: int list =
    line |> String.split_on_char ',' 
         |> List.map int_of_string
  in
  let rulelines = read_lines_until_newline ()
  and updatelines = read_lines_until_newline () in
  let rules = 
    rulelines 
      |> List.to_seq |> Seq.map parse_rule
      |> Seq.fold_left (fun map (k, v) -> 
        let newset = match IntMap.find_opt k map with
          | Some set -> set |> Ints.add v
          | None -> Ints.singleton v
        in 
        map |> IntMap.add k newset
      ) IntMap.empty
  and updates = 
    updatelines |> List.to_seq 
                |> Seq.map parse_update |> Seq.map List.to_seq in
  {
    rules;
    updates;
  }

let unordered_pairs f t: (int * int) Seq.t =
  assert(f <= t);
  Seq.init (t - f) (fun i -> f + i)
    |> Seq.map(fun i -> 
        Seq.init (t - i) (fun j -> (i, i + j + 1))
    ) 
    |> Seq.flat_map (fun x -> x)

let or_else els (opt: 'a option): 'a = match opt with
  | Some v -> v
  | None -> els

let is_valid_update rules (update: int Seq.t): bool =
  let arr = update |> Array.of_seq in
  let pairs = unordered_pairs 0 (Array.length arr - 1) in
  pairs |> Seq.map(fun (i, j) -> (arr.(i), arr.(j)))
    |> Seq.for_all(fun (a, b) -> 
      (* must not have rule j|i *)
      let afterj = rules |> IntMap.find_opt b |> or_else Ints.empty in
      afterj |> Ints.find_opt a |> Option.is_none
    )

let part_one ({ rules; updates } : input) = 
  let result = updates 
    |> Seq.filter (fun upd -> is_valid_update rules upd)
    |> Seq.map(fun upd -> 
      let arr = Array.of_seq upd in
      arr.((Array.length arr) / 2)
    ) |> Seq.fold_left (fun acc v -> acc + v) 0 in
  Printf.printf "Sum of middle numbers: %d\n" result

let part_two ({ rules; updates } : input) =
  let compare a b = 
    let aset = IntMap.find_opt a rules |> or_else Ints.empty 
    and bset = IntMap.find_opt b rules |> or_else Ints.empty in
    if Ints.mem b aset then
      -1
    else if Ints.mem a bset then
      1
    else 
      a - b
  in
  let result = updates 
    |> Seq.filter (fun upd -> not (is_valid_update rules upd))
    |> Seq.map (fun upd -> 
      let arr = Array.of_seq upd in
      Array.stable_sort compare arr;
      arr.((Array.length arr) / 2)
    )
    |> Seq.fold_left (fun acc v -> acc + v) 0 in
  Printf.printf "Total thing: %d\n" result
  
let () = 
  let input = read_input () in
  (* part_one input; *)
  part_two input
