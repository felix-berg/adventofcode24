let read_line_opt () = 
  try Some (read_line ()) with End_of_file -> None

let rec read_lines () = match read_line_opt () with
  | Some line -> line :: read_lines ()
  | None -> []

let lines_to_grid lines: char array array * int * int = 
  let grid = lines |> List.to_seq
    |> Seq.map (fun line -> line |> String.to_seq |> Array.of_seq)
    |> Array.of_seq
  in
  (grid, Array.length grid, Array.length grid.(0))

type input = {
  grid: char array array;
  m: int; 
  n: int;
}

module IntPair = struct
  type t = int * int
  let compare (x1, y1) (x2, y2) =
    match compare x1 x2 with
    | 0 -> compare y1 y2
    | c -> c
end

module CharMap = Map.Make(Char)
module PairSet = Set.Make(IntPair)

let all_grid_indices m n: (int * int) Seq.t = 
  let ident x = x in
  Seq.product (Seq.init m ident) (Seq.init n ident)

let append k v (map: PairSet.t CharMap.t) = 
  let newset = match map |> CharMap.find_opt k with
    | Some set -> set |> PairSet.add v
    | None -> PairSet.singleton v
  in map |> CharMap.add k newset

let get_sets grid m n = 
  all_grid_indices m n 
    |> Seq.fold_left (
      fun (map: PairSet.t CharMap.t) (i, j) -> match grid.(i).(j) with
        | '.' -> map
        | char -> map |> append char (i, j)
    ) CharMap.empty

let read_input () : input =
  let lines = read_lines () in
  let (grid, m, n) = lines_to_grid lines in
  { grid; m; n }

let add_vecs (i1, j1) (i2, j2) = (i1 + i2, j1 + j2)
let sub_vecs (i1, j1) (i2, j2) = (i1 - i2, j1 - j2)
let mul_vec (i, j) k = (i * k, j * k)

let in_bounds m n (i, j) =
  0 <= i && i < m && 0 <= j && j < n

let seq_flatten seq = Seq.flat_map (fun x -> x) seq

let nats = 
  let gen n = Some(n, n + 1) in
  Seq.unfold gen 0

let find_antinodes m n (antennas: PairSet.t): (int * int) Seq.t = 
  let seq = PairSet.to_seq antennas in
  Seq.product seq seq 
    |> Seq.filter (fun (x, y) -> IntPair.compare x y <> 0)
    |> Seq.map (
      fun (x, y) ->
        let dp = sub_vecs x y
        and dn = sub_vecs y x in

        let ps = nats |> Seq.map (
          fun k -> add_vecs x (mul_vec dp k)
        ) |> Seq.take_while (in_bounds m n)
        and ns = nats |> Seq.map (
          fun k -> add_vecs y (mul_vec dn k)
        ) |> Seq.take_while (in_bounds m n) in
      [ ps; ns ] |> List.to_seq |> Seq.concat
    ) |> seq_flatten

let part_two input = 
  let { grid; m; n } = input in
  let map = get_sets grid m n in
  let antinodes = 
    map |> CharMap.to_seq |> Seq.fold_left (
    fun antinodes (_, antennas) -> 
      let new_antinodes = find_antinodes m n antennas
      in antinodes |> PairSet.add_seq new_antinodes
  ) PairSet.empty
  in 
  let result = PairSet.cardinal antinodes in
  Printf.printf "Number of antinodes: %d\n" result

let () = 
  let input = read_input () in
  part_two input

