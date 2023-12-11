let rec read_lines ic =
  try
    let line = input_line ic in
    line :: read_lines ic
  with End_of_file ->
    []

let find_sums lines expansion_size =
  let w = String.length (List.hd lines) in
  let h = List.length lines in
  let empty_cols = Hashtbl.create w in
  let empty_rows = Hashtbl.create h in
  let galaxies = ref [] in
  (* mark all rows and cols as empty first *)
  for i = 0 to w - 1 do
    Hashtbl.add empty_cols i ();
  done;
  for i = 0 to h - 1 do
    Hashtbl.add empty_rows i ();
  done;
  (* iterate to find galaxies *)
  let iter_rows y row =
    let iter_cols x g =
      if g = '#' then begin
        Hashtbl.remove empty_cols x;
        Hashtbl.remove empty_rows y;
        galaxies := (x, y) :: !galaxies
      end in
    String.iteri iter_cols row in
  List.iteri iter_rows lines;
  (* perform expansion *)
  let xarr = Array.make w 0 in
  let yarr = Array.make h 0 in
  let acc = ref 0 in
  for i = 0 to w - 1 do
    if Option.is_some (Hashtbl.find_opt empty_cols i) then
      acc := !acc + expansion_size;
    xarr.(i) <- !acc
  done;
  let acc = ref 0 in
  for i = 0 to h - 1 do
    if Option.is_some (Hashtbl.find_opt empty_rows i) then
      acc := !acc + expansion_size;
    yarr.(i) <- !acc
  done;
  let expand (x, y) = ((x + xarr.(x)), (y + yarr.(y))) in
  let galaxies = List.map expand !galaxies in
  let sum = ref 0 in
  let iter1 i (x1, y1) =
    let iter2 j (x2, y2) =
      if i < j then
        sum := !sum + (abs (x1 - x2)) + (abs (y1 - y2)) in
    List.iteri iter2 galaxies in
  List.iteri iter1 galaxies;
  !sum

let () =
  let ic = open_in "input" in
  let lines = read_lines ic in
  close_in ic;
  print_int (find_sums lines 1);
  print_endline "";
  print_int (find_sums lines 999999);
  print_endline ""
