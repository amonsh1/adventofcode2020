module CharsCounter = Map.Make(Char)

type line_type = (int * int * char * string)

let count_chars (str: string): int CharsCounter.t =
    String.fold_left(fun res chr -> CharsCounter.add chr ((Option.value (CharsCounter.find_opt chr res) ~default:0) + 1) res) CharsCounter.empty str


let parse_lines (lines: string list) =
    let _parse_lines (line: string): line_type =
        let splitted = Str.split(Str.regexp " ") line in
        let (counters, chr, str) = (
            (List.nth splitted 0) , 
            (String.get (String.trim (List.nth splitted 1)) 0) , 
            (String.trim (List.nth splitted 2))
        ) in
        let (min, max) = Str.split(Str.regexp "-") counters |> List.map String.trim |> (fun c -> (List.nth c 0, List.nth c 1)) in
        (int_of_string min , int_of_string max, chr, str)
    in
    List.map _parse_lines lines

(* day1 part 1 *)
module Part1 = struct
    let get_valid_passwords (lines: line_type list): line_type list = 
        let _get_valid_passwords (line: line_type): bool = 
            let (min, max, char, word) = line in
            let count_chars = (count_chars word) in
            let number = Option.value (CharsCounter.find_opt char count_chars) ~default:0 in
            number >= min && number <= max
        in
        List.filter _get_valid_passwords lines

    let test (file_path: string) =
        let input_data_channel = open_in file_path in 
        let passwords = get_valid_passwords (parse_lines (Utils.read_all_lines input_data_channel)) in 
        assert (477 = (List.length passwords))
end


(* day2 part 2 *)
module Part2 = struct
    let get_valid_passwords (lines: line_type list): line_type list = 
        let _get_valid_passwords (line: line_type): bool = 
            let (min, max, char, word) = line in
            List.length (List.filter (fun c -> c == char) [(String.get word (min - 1)); String.get word (max - 1)]) == 1
        in
        List.filter _get_valid_passwords lines

    let test (file_path: string) =
        let input_data_channel = open_in file_path in 
        let passwords = get_valid_passwords (parse_lines (Utils.read_all_lines input_data_channel)) in 
        assert ((List.length passwords) == 686)
end
