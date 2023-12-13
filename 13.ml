let rec read_grids ic list accum =
  try
    let line = input_line ic in
    let list, accum = if line = String.empty then
      ([], accum @ [Array.of_list list])
    else
      (list @ [Array.of_seq (String.to_seq line)], accum) in
    read_grids ic list accum
  with End_of_file ->
    accum @ [Array.of_list list]

let count_difference l1 l2 =
  Array.fold_left (+) 0 (Array.map2 (fun a b -> if a = b then 0 else 1) l1 l2)

let test_horizontal_symmetry grid =
  (* Find a row that is duplicated *)
  let result = ref None in
  for i = 0 to Array.length grid - 2 do
    if grid.(i) = grid.(i + 1) then
      let l = ref (i - 1) in
      let r = ref (i + 2) in
      let is_symmetric = ref true in
      while !l >= 0 && !r < Array.length grid do
        if not (grid.(!l) = grid.(!r)) then
          is_symmetric := false;
        l := !l - 1;
        r := !r + 1;
      done;
      if !is_symmetric then
        result := Some (i + 1)
  done;
  !result

let test_smudged_horizontal_symmetry grid =
  (* Find a row that is duplicated *)
  let result = ref None in
  for i = 0 to Array.length grid - 2 do
    let l = ref (i) in
    let r = ref (i + 1) in
    let smudge_count = ref 0 in
    while !l >= 0 && !r < Array.length grid do
      smudge_count := !smudge_count + (count_difference grid.(!l) grid.(!r));
      l := !l - 1;
      r := !r + 1;
    done;
    if !smudge_count = 1 then
      result := Some (i + 1)
  done;
  !result

let transpose_grid grid =
  let w = Array.length grid in
  let h = Array.length grid.(0) in
  let make_row y =
    let make_col x =
      grid.(x).(y) in
    Array.init w make_col in
  Array.init h make_row

let find_symmetry grid =
  match test_horizontal_symmetry grid with
  | Some value -> (100 * value)
  | None ->
    match test_horizontal_symmetry (transpose_grid grid) with
    | Some value -> value
    | None -> failwith ""

let fix_smudge grid =
  match test_smudged_horizontal_symmetry grid with
  | Some value -> (100 * value)
  | None ->
    match test_smudged_horizontal_symmetry (transpose_grid grid) with
    | Some value -> value
    | None -> failwith ""

let () =
  let ic = open_in "input" in
  let grids = read_grids ic [] [] in
  print_int (List.fold_left (+) 0 (List.map find_symmetry grids));
  print_endline "";
  print_int (List.fold_left (+) 0 (List.map fix_smudge grids));
  print_endline "";
  ()
