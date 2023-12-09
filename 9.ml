let rec measure_slope sequence =
  match sequence with
  | a :: b :: t -> (b - a) :: measure_slope (b :: t)
  | _ -> []

let rec is_all_zero seq =
  match seq with
  | [] -> true
  | 0 :: t -> is_all_zero t
  | _ -> false

let rec extrapolate sequence =
  let value =
    if is_all_zero sequence then
      0
    else
      let next = extrapolate (measure_slope sequence) in
      (List.hd sequence) - (List.hd next) in
  value :: sequence

let extrapolate_forwards sequence = List.hd (extrapolate (List.rev sequence))

let extrapolate_backwards sequence = List.hd (extrapolate sequence)
        
let rec parse_input ic =
  try
    let line = input_line ic in
    let line = List.map int_of_string (String.split_on_char ' ' line) in
    line :: parse_input ic
  with End_of_file ->
    []

let part_1 lines =
  print_int (List.fold_left (+) 0 (List.map extrapolate_forwards lines));
  print_endline ""

let part_2 lines =
  print_int (List.fold_left (+) 0 (List.map extrapolate_backwards lines));
  print_endline ""
        
let () =
  let ic = open_in "input" in
  let lines = parse_input ic in
  close_in ic;
  part_1 lines;
  part_2 lines
