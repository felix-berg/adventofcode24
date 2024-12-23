type part_type = Block | Space

exception No_impl

let read_input () : (int * part_type) Seq.t = 
  let line = read_line () in
  line |> String.to_seqi |> Seq.map(
    fun (i, c) -> 
      let k = int_of_string (String.init 1 (fun _ -> c)) in
      if i mod 2 = 0 then
        (k, Block)
      else
        (k, Space)
  )

type id = Space | Id of int

let to_id_seq (input: (int * part_type) Seq.t) : id Seq.t = 
  input |> Seq.mapi (
    fun index (num, tpe) ->
      let id = index / 2 in
      Seq.init num (fun _ -> (match tpe with Block -> Id id | Space -> Space))
  ) |> Seq.flat_map (fun x -> x)

let compact_ids (arr: id array): int Seq.t = 
  let rec gen (f, t) = 
    if (f > t) then None
    else match (arr.(f), arr.(t)) with
      | (Id id, _) -> Some (id, (f + 1, t))
      | (Space, Id id) -> Some (
        id,
        (f + 1, t - 1)
      )
      | (Space, Space) -> gen (f, t - 1)
  in
  Seq.unfold gen (0, Array.length arr - 1)

let () = 
  let input = read_input () in
  let arr = input |> to_id_seq |> Array.of_seq in
  let check_sum = compact_ids arr |> Seq.mapi (
    fun pos id -> pos * id
  ) |> Seq.fold_left (fun acc v -> acc + v) 0 in
  print_endline (string_of_int check_sum)
