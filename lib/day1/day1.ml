(* day1 part 1 *)
module Part1 = struct
    let find_enteries (lines : string list): int list = 
        let rec _find_enteries (lines : int list) (enteries : int list): int list = 
            match lines with
            | [] -> enteries
            | h::t -> (
                let sum_enteries = List.filter (fun num -> num + h = 2020) lines in
                let new_enteries = List.map (fun num -> num * h) sum_enteries in
                _find_enteries t enteries @ new_enteries
            )
        in
        let int_lines = List.map int_of_string lines in
        _find_enteries int_lines []

    let test (file_path: string) =
        let input_data_channel = open_in file_path in 
        let vars = find_enteries (Utils.read_all_lines input_data_channel) in
        let sum = List.fold_left (+) 0 vars in
        assert (sum = 538464);
        ()
end


(* day1 part 2 *)
module Part2 = struct
    let get_combinations (lines : string list) (count : int): (int list) list = 
        let int_lines = List.map int_of_string lines in
        let lines_length = (List.length lines - 1) in
        let count_iteration = (List.rev (List.init count (fun i -> i))) in 
        let rec _get_combinations (indices: int list) (res_variants: (int list) list) (cnt: int): (int list) list = 
            let (allowed_indices : int list) = List.filter (fun v -> List.nth indices v != lines_length) count_iteration in
            if List.length allowed_indices == 0 then
                res_variants
            else
                let i = List.nth allowed_indices 0 in
                let head = List.filteri (fun index _ -> i > index) indices in
                let tail = List.init (count - i) (fun _ -> (List.nth indices i) + 1) in
                let combination_indecies = (List.append head tail) in 
                let combination = List.map (fun var_index -> List.nth int_lines var_index) combination_indecies in

                _get_combinations combination_indecies (combination::res_variants) (cnt + 1)

        in
        _get_combinations (List.init count (fun _ -> 0)) [(List.init count (fun _ -> List.nth int_lines 0))] 0

    let test (file_path: string) =
        let sum_elems = List.fold_left (+) 0  in
        let input_data_channel = open_in file_path in 
        let combinations = get_combinations (Utils.read_all_lines input_data_channel) 3 in
        let specifyed_combinations = List.nth (List.filter (fun vrs-> (sum_elems vrs) == 2020) combinations) 0 in
        let sum_specifyed_combinations = List.fold_left (fun res v -> res * v) 1  specifyed_combinations in
        assert (sum_specifyed_combinations = 278783190);
        ()
end
