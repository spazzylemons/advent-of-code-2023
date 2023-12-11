let rec read_lines ic =
  try
    let line = input_line ic in
    line :: read_lines ic
  with End_of_file ->
    []

let rec find_galaxies x y rows =
  match rows with
  | row :: t ->
    if x = String.length row then
      find_galaxies 0 (succ y) t
    else if String.unsafe_get row x = '#' then
      (x, y) :: find_galaxies (succ x) y rows
    else
      find_galaxies (succ x) y rows
  | [] -> []

let rec measure_distances galaxies compare accum =
  match galaxies with
  | g :: gs -> begin
    let (x1, y1) = g in
    let (x2, y2) = compare in
    let dist = (abs (x1 - x2)) + (abs (y1 - y2)) in
    measure_distances gs compare (dist + accum)
  end
  | [] -> accum

let rec sum_distances galaxies accum =
  match galaxies with
  | g :: gs -> begin
    let dists = measure_distances gs g 0 in
    sum_distances gs (dists + accum)
  end
  | [] -> accum

let rec create_expansion i accum result empty size =
  if i < Array.length result then begin
    let accum = if empty.(i) then accum + size else accum in
    result.(i) <- accum;
    create_expansion (i + 1) accum result empty size
  end

let find_sums lines expansion_size =
  let w = String.length (List.hd lines) in
  let h = List.length lines in
  (* Find the galaxies. *)
  let galaxies = find_galaxies 0 0 lines in
  (* Iterate to mark rows and columns as not empty. *)
  let empty_cols = Array.make w true in
  let empty_rows = Array.make h true in
  let iter_galaxies (x, y) =
    empty_cols.(x) <- false;
    empty_rows.(y) <- false in
  List.iter iter_galaxies galaxies;
  (* Perform expansion. *)
  let xarr = Array.make w 0 in
  create_expansion 0 0 xarr empty_cols expansion_size;
  let yarr = Array.make h 0 in
  create_expansion 0 0 yarr empty_rows expansion_size;
  let expand (x, y) = ((x + xarr.(x)), (y + yarr.(y))) in
  (* Sum the distances. *)
  sum_distances (List.map expand galaxies) 0

let () =
  let ic = open_in "input" in
  let lines = read_lines ic in
  close_in ic;
  print_int (find_sums lines 1);
  print_endline "";
  print_int (find_sums lines 999999);
  print_endline ""
