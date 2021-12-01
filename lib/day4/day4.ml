module PassportMap = Map.Make (String)

type hgt_type =
  | HgtIn of int
  | HgtCm of int

type ecl_type =
  | Amb
  | Blu
  | Brn
  | Gry
  | Grn
  | Hzl
  | Oth

type field =
  | Byr of int
  | Iyr of int
  | Eyr of int
  | Hgt of hgt_type
  | Hcl of string
  | Ecl of ecl_type
  | Pid of string

type passport =
  { byr : field option
  ; iyr : field option
  ; eyr : field option
  ; hgt : field option
  ; hcl : field option
  ; ecl : field option
  ; pid : field option
  }

let print_field (field_value : field) =
  match field_value with
  | Byr byr_val ->
      Printf.printf "%d\n" byr_val
  | Iyr iyr_val ->
      Printf.printf "%d\n" iyr_val
  | Eyr eyr_val ->
      Printf.printf "%d\n" eyr_val
  | Hgt value ->
    ( match value with
    | HgtIn hgt_val ->
        Printf.printf "%din\n" hgt_val
    | HgtCm hgt_val ->
        Printf.printf "%dcm\n" hgt_val )
  | Hcl hcl_val ->
      Printf.printf "%s\n" hcl_val
  | Ecl _ ->
      print_endline "ecl_val"
  | Pid pid_val ->
      Printf.printf "%s\n" pid_val


let field_validator (field_value : field) =
  match field_value with
  | Byr byr_val ->
      byr_val < 1920 || byr_val > 2002
  | Iyr iyr_val ->
      iyr_val < 2010 || iyr_val > 2020
  | Eyr eyr_val ->
      eyr_val < 2020 || eyr_val > 2030
  | Hgt value ->
    ( match value with
    | HgtIn hgt_val_in ->
        hgt_val_in < 59 || hgt_val_in > 76
    | HgtCm hgt_val_cm ->
        hgt_val_cm < 150 || hgt_val_cm > 193 )
  | Hcl hcl_val ->
      let allowed_vals =
        [ 'a'
        ; 'b'
        ; 'c'
        ; 'd'
        ; 'e'
        ; 'f'
        ; '0'
        ; '1'
        ; '2'
        ; '3'
        ; '4'
        ; '5'
        ; '6'
        ; '7'
        ; '8'
        ; '9'
        ]
      in
      if String.length hcl_val != 7
      then true
      else if not (Char.equal (String.get hcl_val 0) '#')
      then true
      else if not
                (String.for_all
                   (fun c -> List.mem c allowed_vals)
                   (String.sub hcl_val 1 6) )
      then true
      else false
  | Ecl _ ->
      false
  | Pid pid_val ->
      let allowed_vals = [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ] in
      if String.length pid_val != 9
      then true
      else if not (String.for_all (fun c -> List.mem c allowed_vals) pid_val)
      then true
      else false


let str_to_ecl (value : string) : ecl_type option =
  match value with
  | "amb" ->
      Some Amb
  | "blu" ->
      Some Blu
  | "brn" ->
      Some Brn
  | "gry" ->
      Some Gry
  | "grn" ->
      Some Grn
  | "hzl" ->
      Some Hzl
  | "oth" ->
      Some Oth
  | _ ->
      None


let str_to_hgt (value : string) : hgt_type option =
  if String.length value < 3
  then None
  else
    let units = String.sub value (String.length value - 2) 2 in
    let hgt_value = String.sub value 0 (String.length value - 2) in
    match units with
    | "cm" ->
        Some (HgtCm (int_of_string hgt_value))
    | "in" ->
        Some (HgtIn (int_of_string hgt_value))
    | _ ->
        None


let lines_to_passport (lines : string list) : passport =
  let one_line =
    List.fold_left
      (fun conc_line line -> conc_line ^ " " ^ String.trim line)
      ""
      lines
  in
  let one_line = String.trim one_line in
  let fields = Str.split (Str.regexp " ") one_line in
  let fields_key_val : (string * string) list =
    List.map
      (fun fld ->
        Str.split (Str.regexp ":") fld |> fun f -> (List.nth f 0, List.nth f 1)
        )
      fields
  in
  let fields_map : string PassportMap.t =
    List.fold_left
      (fun res fld -> PassportMap.add (fst fld) (snd fld) res)
      PassportMap.empty
      fields_key_val
  in
  { byr =
      PassportMap.find_opt "byr" fields_map
      |> Option.fold ~none:None ~some:(fun a -> Some (Byr (int_of_string a)))
  ; iyr =
      PassportMap.find_opt "iyr" fields_map
      |> Option.fold ~none:None ~some:(fun a -> Some (Iyr (int_of_string a)))
  ; eyr =
      PassportMap.find_opt "eyr" fields_map
      |> Option.fold ~none:None ~some:(fun a -> Some (Eyr (int_of_string a)))
  ; hgt =
      PassportMap.find_opt "hgt" fields_map
      |> Option.fold ~none:None ~some:(fun a ->
             Option.bind (str_to_hgt a) (fun c -> Some (Hgt c)) )
  ; hcl =
      PassportMap.find_opt "hcl" fields_map
      |> Option.fold ~none:None ~some:(fun a -> Some (Hcl a))
  ; ecl =
      PassportMap.find_opt "ecl" fields_map
      |> Option.fold ~none:None ~some:(fun a ->
             Option.bind (str_to_ecl a) (fun c -> Some (Ecl c)) )
  ; pid =
      PassportMap.find_opt "pid" fields_map
      |> Option.fold ~none:None ~some:(fun a -> Some (Pid a))
  }


let parse_lines_to_passports (lines : string list) : passport list =
  let rec _parse_lines_to_passports
      (lines : string list)
      (passports_records : passport list)
      (passports_lines : string list) : passport list =
    match lines with
    | head :: tail when String.equal head "" ->
        _parse_lines_to_passports
          tail
          (lines_to_passport passports_lines :: passports_records)
          []
    | head :: tail ->
        _parse_lines_to_passports
          tail
          passports_records
          (head :: passports_lines)
    | [] ->
        lines_to_passport passports_lines :: passports_records
  in
  _parse_lines_to_passports lines [] []


let get_valid_passports (lines : passport list) =
  let validate_passport { byr; iyr; eyr; hgt; hcl; ecl; pid } =
    List.for_all
      (fun some_field_val ->
        match some_field_val with
        | None ->
            false
        | Some value ->
            let s = not (field_validator value) in
            s )
      [ byr; iyr; eyr; hgt; hcl; ecl; pid ]
  in
  List.filter validate_passport lines


let test (file_path : string) =
  let input_data_channel = open_in file_path in
  let lines = Utils.read_all_lines input_data_channel in
  let passports = get_valid_passports (parse_lines_to_passports lines) in
  let number = List.length passports in
  assert (number == 133)

