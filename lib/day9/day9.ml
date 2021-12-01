let rec find_sum (elems : int list) (num : int) : (int * int) option =
  match elems with
  | h :: t ->
    let list_with_needed_sum = List.filter (fun v -> v + h == num) elems in
    if List.length list_with_needed_sum != 0
    then Some (List.nth list_with_needed_sum 0, h)
    else find_sum t num
  | [] -> None


let find_sum_from_preamble (numbers : int list) (preamble : int) : int =
  let rec _find_sum_from_preamble (tail_numbers : int list) =
    let preview_numbers =
      List.rev (List.filteri (fun i _ -> i < preamble + 1) tail_numbers)
    in
    match preview_numbers with
    | h :: t ->
      (match find_sum t h with
      | Some _ -> _find_sum_from_preamble (List.tl tail_numbers)
      | None -> h)
    | [] -> 0
  in
  _find_sum_from_preamble numbers


let rec find_seq_sum (elems : int list) (sum : int) : int =
  let rec _find_seq_sum (source_elems : int list) (sum_of_chunk : int) (seq : int list)
      : int list
    =
    match source_elems with
    | h :: t when sum_of_chunk < sum ->
      _find_seq_sum t (sum_of_chunk + h) (List.cons h seq)
    | _ -> seq
  in
  match elems with
  | h :: t ->
    let sum_of_seq = _find_seq_sum (h :: t) 0 [] in
    if sum == List.fold_left ( + ) 0 sum_of_seq
    then (
      let min_max_vals =
        List.fold_left
          (fun min_max v -> Int.min (fst min_max) v, Int.max (snd min_max) v)
          (List.nth sum_of_seq 0, List.nth sum_of_seq 0)
          sum_of_seq
      in
      fst min_max_vals + snd min_max_vals)
    else find_seq_sum t sum
  | [] -> 0


let test_part1 file_path =
  let input_data_channel = open_in file_path in
  let lines = List.map int_of_string (Utils.read_all_lines input_data_channel) in
  let c = find_sum_from_preamble lines 25 in
  assert (556543474 == c)


let test_part2 file_path =
  let input_data_channel = open_in file_path in
  let lines = List.map int_of_string (Utils.read_all_lines input_data_channel) in
  let c = find_seq_sum lines 556543474 in
  assert (76096372 == c)
