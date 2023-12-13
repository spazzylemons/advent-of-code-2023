let rec read_grids ic list accum =
  try
    let line = input_line ic in
    let list, accum = if line = String.empty then
      ([], (Array.of_list list) :: accum)
    else
      (list @ [Array.of_seq (String.to_seq line)], accum) in
    read_grids ic list accum
  with End_of_file ->
    (Array.of_list list) :: accum

let rec count_difference l1 l2 i accum =
  if i = Array.length l1 then
    accum
  else if l1.(i) = l2.(i) then
    count_difference l1 l2 (succ i) accum
  else
    count_difference l1 l2 (succ i) (succ accum)

let rec test_exact_symmetry grid l r =
  if l < 0 || r >= Array.length grid then
    true
  else if not (grid.(l) = grid.(r)) then
    false
  else
    test_exact_symmetry grid (pred l) (succ r)

let rec find_exact_symmetry i grid =
  if i = Array.length grid then
    None
  else if test_exact_symmetry grid (pred i) i then
    Some i
  else
    find_exact_symmetry (succ i) grid

let rec test_smudged_symmetry grid l r accum =
  if l < 0 || r >= Array.length grid then
    accum = 1
  else
    let accum = count_difference grid.(l) grid.(r) 0 accum in
    if accum > 1 then
      false
    else
      test_smudged_symmetry grid (pred l) (succ r) accum

let rec find_smudged_symmetry i grid =
  if i = Array.length grid then
    None
  else if test_smudged_symmetry grid (pred i) i 0 then
    Some i
  else
    find_smudged_symmetry (succ i) grid

let transpose_grid grid =
  let w = Array.length grid in
  let h = Array.length grid.(0) in
  let make_row y =
    let make_col x =
      grid.(x).(y) in
    Array.init w make_col in
  Array.init h make_row

let rec calculate f grids accum =
  match grids with
  | g :: gs ->
    let v = match f g with
    | Some value -> (100 * value)
    | None ->
      match f (transpose_grid g) with
      | Some value -> value
      | None -> failwith "" in
    calculate f gs (v + accum)
  | [] -> accum

let part_1 grids = 
  print_int (calculate (find_exact_symmetry 1) grids 0);
  print_endline ""

let part_2 grids =
  print_int (calculate (find_smudged_symmetry 1) grids 0);
  print_endline ""

let () =
  let ic = open_in "input" in
  let grids = read_grids ic [] [] in
  close_in ic;
  part_1 grids;
  part_2 grids
