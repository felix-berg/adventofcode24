type 'a grid = ('a array) array

let read_line_opt (): string option = 
  try
    Some (Stdlib.read_line ())
  with 
    Stdlib.End_of_file -> None

let rec read_lines (): string list = match read_line_opt () with
  | Some (line) -> line :: read_lines ()
  | None -> []

type input = {
  grid: char grid;
  rows: int;
  cols: int;
}

let read_input (): input option =
  let lines = read_lines () in
  assert (List.is_empty lines = false);

  (* all lines have same length *)
  let cols = lines |> List.hd |> String.length in
  let correct_len line = (cols = String.length line) in
  assert (lines |> List.for_all correct_len);

  (* convert string list list to char grid *)
  let chararr_of_str str = str |> String.to_seq |> Array.of_seq in
  let g: char grid = 
    lines |> List.map chararr_of_str
    |> Array.of_list 
  in Some { grid = g; rows = Array.length g; cols }

type indices = (int * int) Seq.t

let create_diagonal rows cols di dj fi fj: indices =
  let step (i, j) = 
    if (0 <= i && i < rows) && (0 <= j && j < cols) then 
      Some((i, j), (i + di, j + dj)) 
    else 
      None
  in
  Seq.unfold step (fi, fj)

let diagonals_indeces rows cols: indices Seq.t =
  let top = Seq.init cols (fun j -> (0, j))
  and left = Seq.init rows (fun i -> (i, 0)) |> Seq.drop 1
  and right = Seq.init rows (fun i -> (i, cols - 1)) |> Seq.drop 1
  and (create_se, create_sw) = (
    (fun (i, j) -> create_diagonal rows cols 1 1 i j),
    (fun (i, j) -> create_diagonal rows cols 1 (-1) i j)
  ) in
  [
    top |> Seq.map create_se;
    left |> Seq.map create_se;
    top |> Seq.map create_sw;
    right |> Seq.map create_sw;
  ] |> List.to_seq |> Seq.concat
  
let hor_vert_indices rows cols: indices Seq.t =
  let row i: indices = Seq.init cols (fun j -> (i, j))
  and col j: indices = Seq.init rows (fun i -> (i, j))
  in
  [ Seq.init rows (fun i -> row i); Seq.init cols (fun j -> col j) ] 
    |> List.to_seq |> Seq.concat

let all_indices rows cols: indices Seq.t =
  [ diagonals_indeces rows cols; hor_vert_indices rows cols ] 
    |> List.to_seq |> Seq.concat

let get_string grid ids: string = 
  ids |> Seq.map (fun (i, j) -> grid.(i).(j))
      |> String.of_seq

let rec list_starts_with ~prefix lst = match (lst, prefix) with
  | (x :: xs, y :: ys) -> x = y && list_starts_with ~prefix:ys xs
  | (_, []) -> true
  | _ -> false

let count_occurances ~sub str =
  let pref = sub |> String.to_seq |> List.of_seq 
  and sublen = String.length sub in
  let drop_pref_len lst = lst |> List.to_seq |> Seq.drop sublen |> List.of_seq in

  let rec cnt_occ lst: int =
    if lst |> list_starts_with ~prefix:pref then
      let rest = drop_pref_len lst in
      1 + cnt_occ rest
    else match lst with
      | _ :: xs -> cnt_occ xs
      | [] -> 0
  in
  cnt_occ (str |> String.to_seq |> List.of_seq)
  
let () = match read_input () with
  | Some { grid; rows; cols } ->
    let strings = 
      all_indices rows cols
        |> Seq.map (fun ids -> 
          ids |> Seq.map (fun (i, j) -> grid.(i).(j))
              |> String.of_seq
        ) 
    in 
    let sum seq = Seq.fold_left (fun acc v -> acc + v) 0 seq 
    and rev_str str = 
      str |> String.to_seq |> List.of_seq 
          |> List.rev |> List.to_seq |> String.of_seq
    in
    let result = 
      strings |> Seq.flat_map (fun str -> [ str ; rev_str str ] |> List.to_seq)
              |> Seq.map (count_occurances ~sub:"XMAS")
              |> sum
    in
    Printf.printf "Total number of occurances: %d\n" result
  | None -> print_endline "invalid input"

