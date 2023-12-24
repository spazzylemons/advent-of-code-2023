let start_pos = 200000000000000
let end_pos = 400000000000000

type vector2 = {
  x : int;
  y : int;
}

type vector3 = {
  x : int;
  y : int;
  z : int;
}

type trail = {
  start : vector3;
  vel   : vector3;
}

type trail2d = {
  start : vector2;

  a : int;
  b : int;
  c : int;
}

let parse_vector3 line =
  match String.split_on_char ',' line with
  | x :: y :: z :: [] ->
    let x = String.trim x |> int_of_string in
    let y = String.trim y |> int_of_string in
    let z = String.trim z |> int_of_string in
    {x = x; y = y; z = z}
  | _ -> failwith ""

let rec read_lines ic =
  try
    let line = input_line ic in
    match String.split_on_char '@' line with
    | start :: vel :: [] ->
      {
        start = parse_vector3 start;
        vel = parse_vector3 vel;
      } :: read_lines ic
    | _ -> failwith ""
  with End_of_file ->
    []

let trail_to_2d trail =
  let a = trail.vel.y in
  let b = -trail.vel.x in
  let c = -(a * trail.start.x + b * trail.start.y) in
  {start = {x = trail.start.x; y = trail.start.y}; a = a; b = b; c = c}

let test_intersection_2d t1 t2 =
  let num = (float_of_int t1.c) *. (float_of_int t2.a) -. (float_of_int t2.c) *. (float_of_int t1.a) in
  let den = (float_of_int t2.b) *. (float_of_int t1.a) -. (float_of_int t1.b) *. (float_of_int t2.a) in
  if den = 0. then
    false
  else begin
    let y = (num) /. (den) in
    let num = (float_of_int t1.c) *. (float_of_int t2.b) -. (float_of_int t2.c) *. (float_of_int t1.b) in
    let x = (num) /. (-.(den)) in
    let f1 = if t1.b > 0 then
      x <= (float_of_int t1.start.x)
    else
      x >= (float_of_int t1.start.x) in
    let f2 = if t2.b > 0 then
      x <= (float_of_int t2.start.x)
    else
      x >= (float_of_int t2.start.x) in
    if f1 && f2 then
      x >= (float_of_int start_pos) &&
      x <= (float_of_int end_pos) &&
      y >= (float_of_int start_pos) &&
      y <= (float_of_int end_pos)
    else false
  end

let rec test_one_hailstone_2d t1 h2d accum =
  match h2d with
  | t2 :: t ->
    let accum = if test_intersection_2d t1 t2 then accum + 1 else accum in
    test_one_hailstone_2d t1 t accum
  | [] -> accum

let rec test_hailstones_2d h2d accum =
  match h2d with
  | x :: xs ->
    let accum = test_one_hailstone_2d x xs accum in
    test_hailstones_2d xs accum
  | [] -> accum

let part_1 hailstones =
  let h2d = hailstones |> List.map trail_to_2d in
  let result = test_hailstones_2d h2d 0 in
  print_int result;
  print_endline ""

let () =
  let ic = open_in "input" in
  let hailstones = read_lines ic in
  close_in ic;
  part_1 hailstones
