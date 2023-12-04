let parse_numbers s =
  List.filter_map int_of_string_opt (String.split_on_char ' ' s)

let list_to_hash l tbl =
  List.iter (fun v -> Hashtbl.add tbl v ()) l

let count_matches numbers winners =
  let predicate n =
    Option.is_some (Hashtbl.find_opt winners n) in
  List.length (List.filter predicate numbers)

let rec read_lines file =
  try
    let line = input_line file in
    line :: read_lines file
  with End_of_file ->
    []

let convert_to_match_count line =
  match String.split_on_char ':' line with
  | _ :: numbers :: [] -> begin
    match (String.split_on_char '|' numbers) with
    | numbers :: winners :: [] ->
      let numbers = parse_numbers numbers in
      let winners = parse_numbers winners in
      let wintbl = Hashtbl.create 32 in
      list_to_hash winners wintbl;
      count_matches numbers wintbl
    | _ -> failwith "Failed to parse"
  end
  | _ -> failwith "Failed to parse"

let count_to_score i =
  Int.shift_right (Int.shift_left 1 i) 1

let part_1 match_count =
  List.fold_left (fun x y -> x + count_to_score y) 0 match_count

let part_2 match_count =
  let length = List.length match_count in
  let counts = Array.make length 1 in
  let match_iter i score =
    if score > 0 then
      let j0 = min (i + 1) (length - 1) in
      let j1 = min (i + score) (length - 1) in
      let c = counts.(i) in
      for j = j0 to j1 do
        counts.(j) <- counts.(j) + c
      done in
  List.iteri match_iter match_count;
  Array.fold_left (+) 0 counts

let () =
  let ic = open_in "input" in
  let match_count = List.map convert_to_match_count (read_lines ic) in
  close_in ic;
  print_int (part_1 match_count);
  print_endline "";
  print_int (part_2 match_count);
  print_endline ""
