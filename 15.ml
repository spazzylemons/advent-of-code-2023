let rec hash' i accum str =
  if i = String.length str then
    accum
  else
    let code = (Char.code (String.unsafe_get str i)) in
    let accum = ((accum + code) * 17) land 255 in
    hash' (succ i) accum str

let hash = hash' 0 0

let rec part1' accum parts =
  match parts with
  | p :: ps ->
    let accum = accum + hash p in
    part1' accum ps
  | [] -> accum

let part1 = part1' 0

let rec remove_from_box box lbl =
  match box with
  | (b, v) :: bs ->
    if b = lbl then bs else (b, v) :: remove_from_box bs lbl
  | [] -> []

let rec add_to_box box lbl value =
  match box with
  | (b, v) :: bs ->
    if b = lbl then
      (b, value) :: bs
    else
      (b, v) :: add_to_box bs lbl value
  | [] -> [(lbl, value)]

let rec focusing_power box i accum =
  match box with
  | (_, v) :: bs ->
    let accum = accum + (v * i) in
    focusing_power bs (i + 1) accum
  | [] -> accum

let rec sum_focusing_power boxes i accum =
  if i = 256 then
    accum
  else
    let accum = accum + (i + 1) * (focusing_power boxes.(i) 1 0) in
    sum_focusing_power boxes (i + 1) accum

let rec part2' parts boxes =
  match parts with
  | p :: ps ->
    let op = (String.unsafe_get p (String.length p - 1)) in
    let lbl, op = if op = '-' then
      (String.sub p 0 (String.length p - 1), '-')
    else
      (String.sub p 0 (String.length p - 2), op) in
    let h = hash lbl in
    boxes.(h) <- begin
      match op with
      | '-' -> remove_from_box boxes.(h) lbl
      | v -> add_to_box boxes.(h) lbl (int_of_char v - 0x30)
    end;
    part2' ps boxes
  | [] -> sum_focusing_power boxes 0 0

let part2 parts =
  let boxes = Array.make 256 [] in
  part2' parts boxes

let () =
  let ic = open_in "input" in
  let line = input_line ic in
  close_in ic;
  let parts = String.split_on_char ',' line in
  print_int (part1 parts);
  print_endline "";
  print_int (part2 parts);
  print_endline ""
