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

module IntMap = Map.Make(IntPair)

let rotate dir = 
  let map = 
    [ (left, up); (up, right); (right, down); (down, left) ] 
    |> List.to_seq |> IntMap.of_seq
  in
  map |> IntMap.find dir

let add_vecs (a: int * int) (b: int * int) = 
  (fst a + fst b, snd a + snd b)

let generate_guard_seq grid m n ~init_state : (int * int) Seq.t =
  let (tl, br) = ((0, 0), (m, n)) in
  let safe_get i j = 
    if in_bounds (i, j) tl br then Some grid.(i).(j) else None
  in

  let next (state: step_state): ((int * int) * step_state) option =
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
      next_state |> Option.map (fun new_state -> (state.guard_pos, new_state))
  in
  Seq.unfold next init_state

let flatten_seq seq = seq |> Seq.flat_map (fun x -> x)

let print_grid grid = 
  grid |> Array.iter (fun line -> 
    Array.iter (fun c -> print_char (char_of_cell c)) line;
    print_newline()
  )

let () = 
  let input = read_input () in
  let mark_pos (i, j) = input.grid.(i).(j) <- Visited in
  let (m, n) = get_grid_size input.grid in
  generate_guard_seq input.grid m n ~init_state:{
    guard_pos = input.guard_pos;
    guard_dir = input.guard_dir;
  } |> Seq.iter mark_pos;
  let res = 
    input.grid 
    |> Array.to_seq |> Seq.map (Array.to_seq) |> flatten_seq
    |> Seq.fold_left (fun acc v -> acc + (if v = Visited then 1 else 0)) 0
  in
  Printf.printf "Number of visited cells: %d\n" res
