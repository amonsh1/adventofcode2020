let parse_lines_to_groups (lines : string list) : string list list list =
  let rec _parse_lines_to_groups
      (lines : string list)
      (group_line : string list list)
      (groups_lines : string list list list)
      : string list list list
    =
    match lines with
    | [] -> List.cons group_line groups_lines
    | head :: tail when String.equal head "" ->
      _parse_lines_to_groups tail [] (List.cons group_line groups_lines)
    | head :: tail ->
      _parse_lines_to_groups
        tail
        (List.cons (Str.split (Str.regexp "") (String.trim head)) group_line)
        groups_lines
  in
  _parse_lines_to_groups lines [] []
;;

module StringsSet = Set.Make (String)

let calc_sum_lengths (groups : string list list list) =
  List.fold_left
    (fun res group_lines ->
      res
      + List.length
          (StringsSet.elements (StringsSet.of_list (List.concat group_lines))))
    0
    groups
;;

let calc_answers_from_all_group (groups : string list list list) =
  List.fold_left
    (fun res group_lines ->
      res
      + List.length
          (StringsSet.elements
             (List.fold_left
                (fun res line -> StringsSet.inter res (StringsSet.of_list line))
                (StringsSet.of_list (List.nth group_lines 0))
                group_lines)))
    0
    groups
;;
let test_part2 file_path =
  let input_data_channel = open_in file_path in
  let lines = Utils.read_all_lines input_data_channel in
  let groups = parse_lines_to_groups lines in
  assert ((calc_answers_from_all_group (groups)) == 3473)
;;

let test_part1 file_path =
  let input_data_channel = open_in file_path in
  let lines = Utils.read_all_lines input_data_channel in
  let groups = parse_lines_to_groups lines in
  assert ((calc_sum_lengths (groups)) == 6911)
;;
