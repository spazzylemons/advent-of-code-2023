let input_filename = "input";;

let rec lstrip str =
  if (String.length str) > 0 && (String.unsafe_get str 0) == ' ' then
    lstrip (String.sub str 1 ((String.length str) - 1))
  else
    str

let rec parse_handful handful =
  match handful with
  | [] -> (0, 0, 0)
  | group :: tail ->
    let group = lstrip group in
    match String.split_on_char ' ' group with
    | num :: (name :: []) ->
      let i = int_of_string num in
      let (tr, tg, tb) = match name with
      | "red"   -> (i, 0, 0)
      | "green" -> (0, i, 0)
      | "blue"  -> (0, 0, i)
      | _       -> failwith "Invalid cube color" in
      let (nr, ng, nb) = parse_handful tail in
      (tr + nr, tg + ng, tb + nb)
    | _ -> failwith "Invalid cube group"

let rec are_handfuls_possible handfuls =
  match handfuls with
  | [] -> true
  | handful :: tail ->
    let (r, g, b) = parse_handful (String.split_on_char ',' handful) in
    if r > 12 || g > 13 || b > 14 then
      false
    else
      are_handfuls_possible tail

let rec find_min_cubes handfuls =
  match handfuls with
  | [] -> (0, 0, 0)
  | handful :: tail ->
    let (tr, tg, tb) = parse_handful (String.split_on_char ',' handful) in
    let (nr, ng, nb) = find_min_cubes tail in
    (max tr nr, max tg ng, max tb nb)

let rec part_1 file =
  try
    let line = input_line file in
    match (String.split_on_char ' ' line) with
    | _ :: (id :: _) ->
      let id = int_of_string (String.sub id 0 ((String.length id) - 1)) in
      let index = ((String.index line ':') + 2) in
      let line = String.sub line index ((String.length line) - index) in
      let handfuls = String.split_on_char ';' line in
      (if are_handfuls_possible handfuls then id else 0) + part_1 file
    | _ -> failwith "Failed to parse game number"
  with End_of_file ->
    0

let rec part_2 file =
  try
    let line = input_line file in
    let index = ((String.index line ':') + 2) in
    let line = String.sub line index ((String.length line) - index) in
    let (r, g, b) = find_min_cubes (String.split_on_char ';' line) in
    (r * g * b) + part_2 file
  with End_of_file ->
    0

let () =
  let ic = open_in input_filename in
  try
    print_int (part_1 ic);
    print_endline "";
    seek_in ic 0;
    print_int (part_2 ic);
    print_endline "";
    close_in ic
  with e ->
    close_in_noerr ic;
    raise e
