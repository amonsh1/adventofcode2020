type command =
  | Acc of int
  | Jmp of int
  | Nop of int

let parse_lines_to_commands (lines : string list) : command list =
  let rec _parse_lines_to_commands
      (lines : string list)
      (commands : command list)
      (arr_index : int)
      : command list
    =
    match lines with
    | [] -> List.rev commands
    | head :: tail ->
      let command, number =
        Str.split (Str.regexp " ") head
        |> fun c -> List.nth c 0, int_of_string (List.nth c 1)
      in
      let new_command =
        match command with
        | "acc" -> Acc number
        | "jmp" -> Jmp number
        | "nop" -> Nop number
        | _ -> failwith "unknown command"
      in
      _parse_lines_to_commands tail (List.cons new_command commands) (arr_index + 1)
  in
  _parse_lines_to_commands lines [] 0


let run_commands (commands : command list) : int * bool =
  let rec _run_commands
      (visited : int list)
      (next_position : int)
      (acc : int)
      (iteration_count : int)
    =
    let acc =
      match List.nth commands (next_position - 1) with
      | Acc num -> num + acc
      | _ -> acc
    in
    let next_position =
      match List.nth commands (next_position - 1) with
      | Jmp num -> next_position + num
      | _ -> next_position + 1
    in
    if List.mem next_position visited || next_position > List.length commands
    then acc, false
    else if List.length commands == next_position
    then acc, true
    else
      _run_commands
        (List.cons next_position visited)
        next_position
        acc
        (iteration_count + 1)
  in
  _run_commands [] 1 0 1


let try_to_fix_program (commands : command list) : int =
  let rec _try_to_fix_program (iteration_count : int) =
    let head_list = List.filteri (fun i _ -> i < iteration_count) commands in
    let tail_list = List.filteri (fun i _ -> i > iteration_count) commands in
    let command_to_replace =
      match List.nth commands iteration_count with
      | Acc num -> Acc num
      | Jmp num -> Nop num
      | command -> command
    in
    let list_with_change = List.append head_list (command_to_replace :: tail_list) in
    let acc, success = run_commands list_with_change in
    if success then acc else _try_to_fix_program (iteration_count + 1)
  in
  _try_to_fix_program 0


let read_all_lines (input_channel : in_channel) : string list =
  let rec _read_all_lines (lines : string list) : string list =
    try
      match input_line input_channel with
      | line -> _read_all_lines (line :: lines)
    with
    | End_of_file -> List.rev lines
  in
  _read_all_lines []


let test_part1 file_path =
  let input_data_channel = open_in file_path in
  let lines = read_all_lines input_data_channel in
  let groups = parse_lines_to_commands lines in
  let acc, _ = run_commands groups in
  assert (acc == 1446)


let test_part2 file_path =
  let input_data_channel = open_in file_path in
  let lines = read_all_lines input_data_channel in
  let groups = parse_lines_to_commands lines in
  let acc = try_to_fix_program groups in
  assert (acc == 1403)
