type action =
| Goto of string
| Reject
| Accept

type comparison =
| LT
| GT

type category =
| X
| M
| A
| S

type rule = {
  cat    : category;
  cmp    : comparison;
  value  : int;
  action : action;
}

type workflow = {
  rules   : rule list;
  default : action;
}

type part = {
  x : int;
  m : int;
  a : int;
  s : int;
}

type range = {
  min: int;
  max: int;
}

type part_range = {
  xr : range;
  mr : range;
  ar : range;
  sr : range;
}

let full_range = {min = 1; max = 4000}

let char_to_cat c =
  match c with
  | 'x' -> X
  | 'm' -> M
  | 'a' -> A
  | 's' -> S
  | _ -> failwith ""

let char_to_cmp c =
  match c with
  | '>' -> GT
  | '<' -> LT
  | _ -> failwith ""

let parse_action s =
  match s with
  | "R" -> Reject
  | "A" -> Accept
  | g -> Goto g

let rec parse_rules rules =
  match rules with
  | a :: b :: t ->
    let cat = char_to_cat (String.get a 0) in
    let cmp = char_to_cmp (String.get a 1) in
    let col = String.index a ':' in
    let value = int_of_string (String.sub a 2 (col - 2)) in
    let action = parse_action (String.sub a (col + 1) (String.length a - col - 1)) in
    let rule = {cat = cat; cmp = cmp; value = value; action = action} in
    let (t, d) = parse_rules (b :: t) in
    (rule :: t), d
  | h :: [] ->
    let action = parse_action h in
    [], action
  | [] -> failwith ""

let parse_workflow line workflows =
  let lb = String.index line '{' in
  let name = String.sub line 0 lb in
  let rules = String.split_on_char ',' (String.sub line (lb + 1) (String.length line - lb - 2)) in
  let rules, default = parse_rules rules in
  Hashtbl.add workflows name {rules = rules; default = default}

let rec parse_workflows ic workflows =
  let line = input_line ic in
  if not (line = "") then begin
    parse_workflow line workflows;
    parse_workflows ic workflows
  end

let parse_part line =
  match String.split_on_char ',' (String.sub line 1 (String.length line - 2)) with
  | x :: m :: a :: s :: [] ->
    let x = int_of_string (String.sub x 2 (String.length x - 2)) in
    let m = int_of_string (String.sub m 2 (String.length m - 2)) in
    let a = int_of_string (String.sub a 2 (String.length a - 2)) in
    let s = int_of_string (String.sub s 2 (String.length s - 2)) in
    {x = x; m = m; a = a; s = s}
  | _ -> failwith ""

let rec parse_parts ic =
  try
    let line = input_line ic in
    let part = parse_part line in
    part :: parse_parts ic
  with End_of_file ->
    []

let rec run_rules part rules =
  match rules with
  | rule :: rules ->
    let value =
      match rule.cat with
      | X -> part.x
      | M -> part.m
      | A -> part.a
      | S -> part.s in
    let pass =
      match rule.cmp with
      | LT -> value < rule.value
      | GT -> value > rule.value in
    if pass then
      Some (rule.action)
    else
      run_rules part rules
  | [] -> None

let rec qc_test name part workflows =
  print_endline name;
  let workflow = Hashtbl.find workflows name in
  let action = match run_rules part workflow.rules with
  | Some action -> action
  | None -> workflow.default in
  match action with
  | Goto name -> qc_test name part workflows
  | Reject -> false
  | Accept -> true

let total_rating part =
  part.x + part.m + part.a + part.s

let rec part_1 parts workflows accum =
  match parts with
  | part :: parts ->
    let accum = if qc_test "in" part workflows then
      accum + total_rating part
    else
      accum in
    part_1 parts workflows accum
  | [] -> accum

let replace_range prange nrange cat =
  match cat with
  | X -> {xr = nrange; mr = prange.mr; ar = prange.ar; sr = prange.sr}
  | M -> {xr = prange.xr; mr = nrange; ar = prange.ar; sr = prange.sr}
  | A -> {xr = prange.xr; mr = prange.mr; ar = nrange; sr = prange.sr}
  | S -> {xr = prange.xr; mr = prange.mr; ar = prange.ar; sr = nrange}

let measure_range range = range.max - range.min + 1

let measure_part_range prange =
  measure_range prange.xr *
  measure_range prange.mr *
  measure_range prange.ar *
  measure_range prange.sr

let rec qc_goto prange workflows name =
  let workflow = Hashtbl.find workflows name in
  qc_test_rules prange workflows workflow.rules workflow.default
and run_action prange workflows action =
  match action with
  | Goto name -> qc_goto prange workflows name
  | Reject -> 0
  | Accept -> measure_part_range prange
and qc_test_rules prange workflows rules default =
  match rules with
  | rule :: rules ->
    let range =
      match rule.cat with
      | X -> prange.xr
      | M -> prange.mr
      | A -> prange.ar
      | S -> prange.sr in
    let trange, frange = match rule.cmp with
    | LT ->
      if range.max >= rule.value then
        if range.min < rule.value then
          let trange = replace_range prange {min = range.min; max = rule.value - 1} rule.cat in
          let frange = replace_range prange {min = rule.value; max = range.max} rule.cat in
          Some trange, Some frange
        else
          None, Some prange
      else
        Some prange, None
    | GT ->
      if range.min <= rule.value then
        if range.max > rule.value then
          let trange = replace_range prange {min = rule.value + 1; max = range.max} rule.cat in
          let frange = replace_range prange {min = range.min; max = rule.value} rule.cat in
          Some trange, Some frange
        else
          None, Some prange
      else
        Some prange, None in
    let tresult =
      match trange with
      | Some prange -> run_action prange workflows rule.action
      | None -> 0 in
    let fresult =
      match frange with
      | Some prange -> qc_test_rules prange workflows rules default
      | None -> 0 in
    tresult + fresult
  | [] -> run_action prange workflows default

let part_2 workflows =
  qc_goto {xr = full_range; mr = full_range; ar = full_range; sr = full_range} workflows "in"

let () =
  let ic = open_in "input" in
  let workflows = Hashtbl.create 1024 in
  parse_workflows ic workflows;
  let parts = parse_parts ic in
  close_in ic;
  print_int (part_1 parts workflows 0);
  print_endline "";
  print_int (part_2 workflows);
  print_endline ""
