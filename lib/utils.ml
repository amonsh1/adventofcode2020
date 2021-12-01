let read_all_lines (input_channel : in_channel) : string list =
  let rec _read_all_lines (lines : string list) : string list =
    try
      match input_line input_channel with
      | line -> _read_all_lines (line :: lines)
    with
    | End_of_file -> List.rev lines
  in
  _read_all_lines []
;;