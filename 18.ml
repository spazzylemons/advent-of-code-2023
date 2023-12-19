type dir =
| Up
| Right
| Down
| Left

type instruction = {
  dir : dir;
  amt : int;
}

type hline = {
  x1 : int;
  x2 : int;
}

let rec read_lines ic =
  try
    match String.split_on_char ' ' (input_line ic) with
    | dir :: amt :: color :: [] -> begin
      let dir1 = match dir with
      | "U" -> Up
      | "R" -> Right
      | "D" -> Down
      | "L" -> Left
      | _ -> failwith "" in
      let dir2 = match (String.sub color 7 1) with
      | "0" -> Up
      | "1" -> Right
      | "2" -> Down
      | "3" -> Left
      | _ -> failwith "" in
      let amt1 = int_of_string amt in
      let amt2 = Scanf.sscanf (String.sub color 2 5) "%x" (fun x -> x) in
      let ins1 = {dir = dir1; amt = amt1} in
      let ins2 = {dir = dir2; amt = amt2} in
      (ins1, ins2) :: (read_lines ic)
    end
    | _ -> failwith ""
  with End_of_file ->
    []

let rec find_range_within a b new_hlines =
  match new_hlines with
  | h :: t ->
    if h.x1 = a.x2 && h.x2 = b.x1 then
      (Some {x1 = a.x1; x2 = b.x2}, t)
    else
      let (x, y) = find_range_within a b t in
      x, h :: y
  | [] -> (None, [])

let rec run_tests_on test a new_hlines =
  match new_hlines with
  | h :: t ->
    begin
      match test a h with
      | Some v -> (Some v, t)
      | None ->
        let (x, y) = run_tests_on test a t in
        x, h :: y
    end
  | [] -> (None, [])

let rec find_extension_left a h =
  if h.x2 = a.x1 then
    Some {x1 = h.x1; x2 = a.x2}
  else None

let rec find_extension_right a h =
  if h.x1 = a.x2 then
    Some {x1 = a.x1; x2 = h.x2}
  else None

let rec find_subtraction_left a h =
  if h.x1 = a.x1 then
    Some {x1 = h.x2; x2 = a.x2}
  else None

let rec find_subtraction_right a h =
  if h.x2 = a.x2 then
    Some {x1 = a.x1; x2 = h.x1}
  else None

let rec merge_ranges list =
  match list with
  | a :: b :: t -> begin
    if a.x2 = b.x1 then
      merge_ranges ({x1 = a.x1; x2 = b.x2} :: t)
    else
      a :: merge_ranges (b :: t)
  end
  | l -> l

let rec find_splitter a new_hlines =
  match new_hlines with
  | h :: t ->
    if h.x1 > a.x1 && h.x2 < a.x2 then
      (Some ({x1 = a.x1; x2 = h.x1}, {x1 = h.x2; x2 = a.x2}), t)
    else
      let (x, y) = find_splitter a t in
      x, h :: y
  | [] -> (None, [])

let rec one_more_splitter old_hlines new_hlines =
  match old_hlines with
  | a :: t -> begin
    let c, h = find_splitter a new_hlines in
    match c with
    | Some (c, d) -> one_more_splitter (c :: d :: t) h
    | None ->
      let x, h = one_more_splitter t h in
      a :: x, h
  end
  | l -> (l, new_hlines)

let rec run_finder finder old_hlines new_hlines =
  match old_hlines with
  | a :: t -> begin
    let c, h = run_tests_on finder a new_hlines in
    match c with
    | Some c -> run_finder finder (c :: t) h
    | None ->
      let x, h = run_finder finder t h in
      a :: x, h
  end
  | l -> (l, new_hlines)

let rec range_of_hlines ranges accum =
  match ranges with
  | range :: ranges ->
    let accum = accum + (range.x2 - range.x1) + 1 in
    range_of_hlines ranges accum
  | [] -> accum

let add_hline_to_table hline_groups y hline =
  match Hashtbl.find_opt hline_groups y with
  | None ->
    Hashtbl.add hline_groups y [hline]
  | Some group ->
    let head = List.hd group in
    if head.x1 < hline.x1 then
      Hashtbl.replace hline_groups y (group @ [hline])
    else
      Hashtbl.replace hline_groups y ([hline] @ group) 

let create_hlines stream =
  let current_pos = ref (0, 0) in
  let hline_groups = Hashtbl.create 65536 in
  let iterator (value : instruction) =
    let x, y = !current_pos in
    match value.dir with
    | Up ->
      current_pos := (x, y - value.amt);
    | Right ->
      current_pos := (x + value.amt, y);
      let hline = {x1 = x; x2 = x + value.amt} in
      add_hline_to_table hline_groups y hline
    | Down ->
      current_pos := (x, y + value.amt);
    | Left ->
      current_pos := (x - value.amt, y);
      let hline = {x1 = x - value.amt; x2 = x} in
      add_hline_to_table hline_groups y hline
      in
  List.iter iterator stream;
  Hashtbl.to_seq hline_groups
  |> List.of_seq
  |> List.sort (fun (k1, v1) (k2, v2) -> compare k1 k2)

let rec result_iterator groups accum current_hlines ly =
  match groups with
  | (y, group) :: groups -> begin
    let size = range_of_hlines current_hlines 0 in
    let accum = accum + (size * (y - ly - 1)) in
    let a, b = current_hlines, group in
    let a, b = run_finder find_extension_left a b in
    let a, b = run_finder find_extension_right a b in
    let a = List.sort (fun a b -> compare a.x1 b.x1) a in
    let a = merge_ranges a in
    let foo = ref [] in
    let bar = ref a in
    let iter2 x =
      if Option.is_none (List.find_opt (fun y -> x.x2 >= y.x1 && x.x1 <= y.x2) !bar) then
        bar := x :: !bar
      else
        foo := x :: !foo in
    List.iter iter2 b;
    let a = !bar in
    let b = !foo in
    let accum = accum + range_of_hlines a 0 in
    let a, b = run_finder find_subtraction_left a b in
    let a, b = run_finder find_subtraction_right a b in
    let a, b = one_more_splitter a b in
    let foo = ref [] in
    let iter3 x =
      if not (x.x1 = x.x2) then
        foo := x :: !foo in
    List.iter iter3 a;
    result_iterator groups accum !foo y
  end
  | [] -> accum

let calculate stream =
  let hlines = create_hlines stream in
  let result = result_iterator hlines 0 [] 0 in
  print_int result;
  print_endline ""

let () =
  let ic = open_in "input" in
  let instructions = read_lines ic in
  close_in ic;
  calculate (List.map fst instructions);
  calculate (List.map snd instructions)
