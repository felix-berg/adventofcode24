type cell = Wall | Unvisited | Visited | Guard of (int * int)

let (down, right, up, left) = ( (1, 0), (0, 1), (-1, 0), (0, -1) )

type input = {
  grid: cell array array;
  guard_pos: (int * int);
  guard_dir: (int * int);
}

let read_line_opt () = 
  try Some (read_line ()) with End_of_file -> None

let rec read_lines (): string list = match read_line_opt () with
  | Some line -> line :: read_lines ()
  | None -> []

let cell_of_char ch: cell = match ch with
  | '.' -> Unvisited
  | '^' -> Guard up
  | '>' -> Guard right
  | '<' -> Guard left
  | 'v' -> Guard down
  | '#' -> Wall
  | _ -> raise Exit

let rec search_in_matrix (pred: 'a -> bool) (searchindices: (int * int) Seq.t) matrix =
  match searchindices () with
  | Seq.Cons ((i, j), _) when pred matrix.(i).(j) -> 
    Some (i, j)
  | Seq.Cons (_, rest) -> search_in_matrix pred rest matrix
  | Seq.Nil -> None

let opt_or_else (els: unit -> 'a) (opt: 'a option): 'a = match opt with
  | Some v -> v
  | None -> els ()

let parse_grid (lines: string list): cell array array =
  let grid = lines |> List.to_seq
    |> Seq.map String.to_seq
    |> Seq.map (Seq.map cell_of_char)
    |> Seq.map Array.of_seq
    |> Array.of_seq in
  let valid: bool = (Array.length grid = 0) ||
      let cols = Array.length grid.(0) in
      Array.for_all (fun row -> (Array.length row) = cols) grid in
  assert valid;
  grid

let get_grid_size grid: (int * int) =
  let m = Array.length grid in
  let n = if m = 0 then 0 else Array.length grid.(0) in
  (m, n)

let all_grid_indices m n =  
  let ident x = x in (* identity function *)
  Seq.product
    (Seq.init m ident)
    (Seq.init n ident)

let read_input (): input = 
  let grid = read_lines () |> parse_grid in
  let (m, n) = get_grid_size grid in
  let allindices = all_grid_indices m n in
  let is_guard cell = match cell with 
    | Guard _ -> true 
    | _ -> false in
  let (gi, gj) = 
    search_in_matrix is_guard allindices grid
      |> opt_or_else (fun _ -> raise Stdlib.Exit)
  in
  let dir = match grid.(gi).(gj) with 
    | Guard d -> d
    | _ -> (raise Exit)
  in
  grid.(gi).(gj) <- Unvisited; (* remove guard cell *)
  { grid; guard_pos = (gi, gj); guard_dir = dir }

let char_of_cell cell = match cell with
  | Unvisited -> '.'
  | Visited -> 'X'
  | Wall -> '#'
  | Guard d -> match d with
    | d when d = left -> '<'
    | d when d = right -> '>'
    | d when d = up -> '^'
    | d when d = down -> 'v'
    | _ -> '?'

type step_state = {
  guard_pos: (int * int);
  guard_dir: (int * int);
}

(* top-left inclusive, bottom-right exclusive *)
let in_bounds p tr bl =
  let (fi, fj) = tr 
  and (ti, tj) = bl in
  (fst p >= fi && fst p < ti) &&
  (snd p >= fj && snd p < tj)

module IntPair = struct
  type t = int * int
  let compare (x1, y1) (x2, y2) =
    match compare x1 x2 with
    | 0 -> compare y1 y2
    | c -> c
end

module IntPairMap = Map.Make(IntPair)

let rotate dir = 
  let map = 
    [ (left, up); (up, right); (right, down); (down, left) ] 
    |> List.to_seq |> IntPairMap.of_seq
  in
  map |> IntPairMap.find dir

let add_vecs (a: int * int) (b: int * int) = 
  (fst a + fst b, snd a + snd b)

let generate_guard_seq grid m n ~init_state : step_state Seq.t =
  let (tl, br) = ((0, 0), (m, n)) in
  let safe_get i j = 
    if in_bounds (i, j) tl br then Some grid.(i).(j) else None
  in

  let next (state: step_state): (step_state * step_state) option =
    if not (in_bounds state.guard_pos tl br) then 
      None
    else 
      let (fi, fj) = add_vecs state.guard_pos state.guard_dir in
      let next_state = match safe_get fi fj with 
        | Some Wall -> 
          (* rotate to the right, move one forward *)
          let nd = rotate state.guard_dir in
          Some {
            guard_pos = add_vecs state.guard_pos nd;
            guard_dir = nd
          }
        | _ -> Some {
          (* move forward, unchanged direction *)
          guard_pos = (fi, fj);
          guard_dir = state.guard_dir;
        } 
      in
      next_state |> Option.map (fun new_state -> (state, new_state))
  in
  Seq.unfold next init_state


(* create a range of ints [f, t) *)
let interval_seq (f: int) (t: int): int Seq.t = 
  Seq.init (t - f) (fun i -> f + i)

let flatten_seq seq = seq |> Seq.flat_map (fun x -> x)

type cycle = {
  tl: int * int; (* top left *)
  br: int * int; (* bottom right *)
}

let get_cycle_points cycle = 
  let tr = (fst cycle.tl, snd cycle.br) 
  and bl = (fst cycle.br, snd cycle.tl) in
  (cycle.tl, tr, cycle.br, bl)

let all_possible_cycles m n: cycle Seq.t =
  let indices_in_sub_matrix fi fj ti tj = 
    if (fi >= ti || fj >= tj) then Seq.empty
    else 
      Seq.product (interval_seq fi ti) (interval_seq fj tj)
  in
  indices_in_sub_matrix 0 0 m n 
    |> Seq.map(fun (fi, fj) -> 
      indices_in_sub_matrix (fi + 1) (fj + 1) m n
        |> Seq.map(fun br -> 
          { tl = (fi, fj); br  }
        )
    ) 
    |> flatten_seq

type almost_cycle = {
  cycle: cycle;
  missing_point: int * int;
}

let matrix_get_opt i j mat = 
  if Array.length mat = 0 then None 
  else
    let (m, n) = (Array.length mat, Array.length mat.(0)) in
    if not (in_bounds (i, j) (0, 0) (m, n)) then
      None 
    else 
      Some mat.(i).(j)

let indices_in_cycle cycle =
  let on_row i seq = seq |> Seq.map (fun j -> (i, j))
  and on_col j seq = seq |> Seq.map (fun i -> (i, j))
  and { tl = (fi, fj); br = (ti, tj) } = cycle in
  [
    interval_seq fi (ti + 1) |> on_col fj; (* left line *)
    interval_seq fi (ti + 1) |> on_col tj; (* right line *)
    interval_seq (fj + 1) tj |> on_row fi; (* top line *) 
    interval_seq (fj + 1) tj |> on_row ti; (* bottom line *)
  ] |> List.to_seq |> flatten_seq

let cycle_is_unobstructed grid cycle =
  indices_in_cycle cycle 
    |> Seq.filter_map (fun (i, j) -> matrix_get_opt i j grid)
    |> Seq.for_all (fun c -> c <> Wall)

let find_almost_cycles grid m n: almost_cycle Seq.t =
  let is_wall i j = (matrix_get_opt i j grid = Some Wall)
  and all_cycles = all_possible_cycles m n |> Seq.filter (cycle_is_unobstructed grid) in
  let missing_walls (cycle: cycle) =
  let { tl = (fi, fj); br = (ti, tj) } = cycle in
    [ (fi - 1, fj); (fi, tj + 1); (ti + 1, tj); (ti, fj - 1) ]
      |> List.filter (fun p -> in_bounds p (0, 0) (m, n)) 
      |> List.filter (fun (i, j) -> not (is_wall i j))
  in
  all_cycles 
    |> Seq.filter_map(fun cycle -> 
      match missing_walls cycle with 
        | [ p ] -> Some ({ cycle; missing_point = p } : almost_cycle)
        | _ -> None
    ) 

type dir_enum = Left | Right | Down | Up

(* get a line ((fi, fj), (ti, j)) corresponding to a side of the cycle *)
let get_line (dir: dir_enum) (cycle: cycle): ((int * int) * (int * int)) = 
  let (tl, br) = (cycle.tl, cycle.br) in
  let tr = (fst tl, snd br) 
  and bl = (fst br, snd tl) in
  match dir with
    | Left -> (tl, bl)
    | Right -> (tr, br)
    | Down -> (bl, br)
    | Up -> (tl, tr)


let guard_is_on_cycle (state: step_state) (cycle: cycle) = 
  (* find line f -> t to bound state.guard_pos to *)
  let (f, t) = match state.guard_dir with
    | dir when dir = up    -> get_line Left cycle
    | dir when dir = left  -> get_line Down cycle
    | dir when dir = right -> get_line Up cycle
    | dir when dir = down  -> get_line Right cycle
    | _ -> 
      raise Exit
  in
  let exclt = add_vecs t (1, 1) in
  in_bounds state.guard_pos f exclt


module Cycle = struct 
  type t = cycle
  let compare c1 c2 =
    let c = IntPair.compare c1.tl c2.tl in
    if c = 0 then IntPair.compare c1.br c2.br else c
end

module CycleSet = Set.Make(Cycle)
module CycleMap = Map.Make(Cycle)

type overlapping_cycles_impl = {
  found: CycleSet.t;
  remaining: CycleSet.t
}

let find_overlapping_cycles (cycles: cycle Seq.t) (guard_seq: step_state Seq.t): CycleSet.t =
  let folder acc (state: step_state) =
    let { found; remaining } = acc in
    match remaining |> CycleSet.find_first_opt (guard_is_on_cycle state) with
      | Some cycle -> {
        found = found |> CycleSet.add cycle;
        remaining = remaining |> CycleSet.remove cycle;
      }
      | None -> acc
  in
  let init: overlapping_cycles_impl = {
    found = CycleSet.empty;
    remaining = cycles |> CycleSet.of_seq;
  } in
  let result = guard_seq |> Seq.fold_left folder init in
  result.found

let part_one input =
  let mark_pos (i, j) = input.grid.(i).(j) <- Visited in
  let (m, n) = get_grid_size input.grid in
  generate_guard_seq input.grid m n ~init_state:{
    guard_pos = input.guard_pos;
    guard_dir = input.guard_dir;
  } |> Seq.map (fun state -> state.guard_pos) |> Seq.iter mark_pos;
  let res = 
    input.grid 
    |> Array.to_seq |> Seq.map (Array.to_seq) |> flatten_seq
    |> Seq.fold_left (fun acc v -> acc + (if v = Visited then 1 else 0)) 0
  in
  Printf.printf "Number of visited cells: %d\n" res
  
let almost_cycles_to_map almost_cycles: (int * int) CycleMap.t = 
  almost_cycles |> Seq.map (fun { cycle; missing_point = p } -> 
    (cycle, p)
  ) |> CycleMap.of_seq

module PairSet = Set.Make(IntPair)

let part_two input = 
  let (m, n) = get_grid_size input.grid in
  print_newline ();
  let map = 
    find_almost_cycles input.grid m n |> almost_cycles_to_map in
  let cycles = 
    map |> CycleMap.to_seq 
        |> Seq.map (fun (cycle, _) -> cycle) 
        |> CycleSet.of_seq in
  let init = { guard_pos = input.guard_pos; guard_dir = input.guard_dir } in
  let steps = generate_guard_seq input.grid m n ~init_state:init in
  let cycles = find_overlapping_cycles (cycles |> CycleSet.to_seq) steps in
  let points = cycles |> CycleSet.to_seq 
      |> Seq.filter_map (fun cycle -> map |> CycleMap.find_opt cycle)
      |> PairSet.of_seq in
  points |> PairSet.iter (fun (i, j) -> 
    Printf.printf "(%d, %d) " i j;
    print_newline ()
  )

let () = 
  let input = read_input() in
  part_two input
