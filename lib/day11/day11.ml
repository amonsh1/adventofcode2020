

let read_all_lines_to_chars (input_channel : in_channel) : char list list =
  let rec _read_all_lines (lines : char list list) : char list list =
    try
      match input_line input_channel with
      | line -> _read_all_lines (List.rev ((String.fold_left (fun res c -> c::res) [] line)):: lines)
    with
    | End_of_file -> List.rev lines
  in
  _read_all_lines []
;;

let process_data (places : char list list) : char list list =
  let around_places = [ -1, -1; 0, -1; 1, -1; -1, 0; 1, 0; -1, 1; 0, 1; 1, 1 ] in
  let get_allowed_neighbors_positions (index : int) (row_count : int) =
    (* Позиции вокруг (x, y), которые не выходят за рамки поля *)
    List.filter
      (fun pos ->
        row_count + snd pos >= 0
        && row_count + snd pos < List.length places
        && index + fst pos >= 0
        && index + fst pos < List.length (List.nth places 0))
      around_places
  in
  let occupied_neighbors_count (cur_pos : int * int) : int =
    (* Сколько соседей neighbor вокруг позиции  cur_pos *)
    let around_places = get_allowed_neighbors_positions (fst cur_pos) (snd cur_pos) in
    List.map
      (fun (x, y) -> List.nth (List.nth places (y + snd cur_pos)) (x + fst cur_pos))
      around_places
    |> List.filter (fun _neighbor -> Char.equal '#' _neighbor)
    |> List.length
  in
  let process_row (row : char list) (row_count : int) =
    List.fold_left
      (fun (new_row : char list) (place : char) ->
        let index = List.length new_row in
        let new_row =
          match place with
          | '#' when occupied_neighbors_count (index, row_count) >= 4 -> List.cons 'L' new_row
          | 'L' when occupied_neighbors_count (index, row_count) == 0 -> List.cons '#' new_row
          | _ -> List.cons place new_row
        in
        new_row)
      []
      row
    |> List.rev
  in
  List.fold_left
    (fun res row -> List.cons (process_row row (List.length res)) res)
    []
    places
  |> List.rev
;;

let rec run preview_state : int=
  let new_state = process_data preview_state in
  if List.equal Char.equal (List.flatten preview_state) (List.flatten new_state)
  then 
    List.length (List.flatten new_state |> List.filter (Char.equal '#'))
  else run new_state
;;

let test_part1 (path: string) =
  let input_data_channel = open_in path in
  
  let lines = read_all_lines_to_chars input_data_channel in
  assert ((run lines) == 2338)
;;
