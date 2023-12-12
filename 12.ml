let rec read_lines ic =
  try
    let line = input_line ic in
    line :: read_lines ic
  with End_of_file ->
    []

let rec make_group n =
  if n = 0 then
    ""
  else
    "#" ^ (make_group (n - 1))

let group_strings = Array.init 50 make_group

let rec test_validity a l i =
  if i = String.length a then
    true
  else
    let c = String.unsafe_get l i in
    if c != '?' && c != String.unsafe_get a i then
      false
    else
      test_validity a l (succ i)

let permutation_cache = Hashtbl.create 65536

let rec permute_recursive compare start groups remaining =
  if test_validity (String.concat "." start) compare 0 then
    let key = (List.length groups, remaining) in
    match Hashtbl.find_opt permutation_cache key with
    | None -> begin
        let result = match groups with
        | h :: t -> begin
          let g = group_strings.(h) in
          if remaining > 0 then
            let opt1 = start @ [""] in
            let opt2 = start @ [g] in
            let g1 = permute_recursive compare opt1 (h :: t) (remaining - 1) in
            let g2 = permute_recursive compare opt2 t (remaining) in
            g1 + g2
          else permute_recursive compare (start @ [g]) t (remaining)
        end
        | [] -> if remaining > 0 then
          permute_recursive compare (start @ [""]) [] (remaining - 1)
        else
          1 in
        Hashtbl.add permutation_cache key result;
        result
      end
    | Some value -> value
  else
    0

let permute2 line groups =
  let groups = List.map int_of_string (String.split_on_char ',' groups) in
  let remaining = (String.length line) - (List.fold_left (+) 0 groups) - (List.length groups - 1) in
  permute_recursive line [] groups remaining

let permute unfolding line =
  Hashtbl.reset permutation_cache;
  match String.split_on_char ' ' line with
  | line :: groups :: [] ->
    if unfolding then
      let line = line ^ "?" ^ line ^ "?" ^ line ^ "?" ^ line ^ "?" ^ line in
      let groups = groups ^ "," ^ groups ^ "," ^ groups ^ "," ^ groups ^ "," ^ groups in
      permute2 line groups
    else
      permute2 line groups
  | _ -> failwith ""

let () =
  let ic = open_in "input" in
  let lines = read_lines ic in
  close_in ic;
  print_int (List.fold_left (+) 0 (List.map (permute false) lines));
  print_endline "";
  print_int (List.fold_left (+) 0 (List.map (permute true) lines));
  print_endline ""
