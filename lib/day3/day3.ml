(* day3 *)
module Part1 = struct
  let get_trees_number
      (lines : string list)
      (step_right_size : int)
      (step_down_size : int)
      : int
    =
    let (row_length : int) =
      String.fold_left (fun len_res _ -> len_res + 1) 0 (List.nth lines 0)
    in
    let lines = List.filteri (fun i _ -> i mod step_down_size == 0) lines in
    let rec _get_trees_number (lines : string list) (position : int) (count : int) : int =
      match lines with
      | head :: tail ->
        if String.get head position == '#'
        then
          _get_trees_number tail ((position + step_right_size) mod row_length) (count + 1)
        else _get_trees_number tail ((position + step_right_size) mod row_length) count
      | [] -> count
    in
    _get_trees_number lines 0 0
  ;;

  let test (file_path : string) =
    let input_data_channel = open_in file_path in
    let lines = Utils.read_all_lines input_data_channel in
    let trees_numbers =
      List.map
        (fun coords -> get_trees_number lines (fst coords) (snd coords))
        [ 1, 1; 3, 1; 5, 1; 7, 1; 1, 2 ]
    in
    assert (List.fold_left (fun res i -> res * i) 1 trees_numbers == 2224913600)
  ;;

  let test2 (file_path : string) =
    let input_data_channel = open_in file_path in
    let lines = Utils.read_all_lines input_data_channel in
    let trees_numbers =
      List.map (fun coords -> get_trees_number lines (fst coords) (snd coords)) [ 3, 1 ]
    in
    assert (List.fold_left (fun res i -> res * i) 1 trees_numbers == 259)
  ;;
end
