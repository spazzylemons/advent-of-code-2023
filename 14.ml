let rec read_grid ic =
  try
    let line = input_line ic in
    let line = Array.of_seq (String.to_seq line) in
    line :: read_grid ic
  with End_of_file ->
    []

let rec shifty grid d y x i =
  if i > 0 then begin
    grid.(y).(x) <- 'O';
    shifty grid d (y + d) x (pred i)
  end

let rec shiftx grid d y x i =
  if i > 0 then begin
    grid.(y).(x) <- 'O';
    shiftx grid d y (x + d) (pred i)
  end

let rec tilty grid s d y x rocks =
  if y < 0 || y >= Array.length grid then begin
    shifty grid d (y + d) x rocks;
    if x < Array.length grid - 1 then
      tilty grid s d s (succ x) 0
  end else
    let c = grid.(y).(x) in
    let rocks = if c = 'O' then begin
      grid.(y).(x) <- '.';
      succ rocks
    end else if c = '#' then begin
      shifty grid d (y + d) x rocks;
      0
    end else rocks in
      tilty grid s d (y - d) x rocks

let rec tiltx grid s d y x rocks =
  if x < 0 || x >= Array.length grid.(0) then begin
    shiftx grid d y (x + d) rocks;
    if y < Array.length grid - 1 then
      tiltx grid s d (succ y) s 0
  end else
    let c = grid.(y).(x) in
    let rocks = if c = 'O' then begin
      grid.(y).(x) <- '.';
      succ rocks
    end else if c = '#' then begin
      shiftx grid d y (x + d) rocks;
      0
    end else rocks in
      tiltx grid s d y (x - d) rocks

let tiltn grid = tilty grid (Array.length grid - 1) 1 (Array.length grid - 1) 0 0
let tilts grid = tilty grid 0 (-1) 0 0 0
let tilte grid = tiltx grid 0 (-1) 0 0 0
let tiltw grid = tiltx grid (Array.length grid.(0) - 1) 1 0 (Array.length grid.(0) - 1) 0

let copy_grid grid = Array.map Array.copy grid

let print_grid grid =
  Array.iter (fun a -> Array.iter print_char a; print_endline "") grid;
  print_endline ""

let rec hash_grid grid x y h =
  if x = Array.length grid.(0) then
    hash_grid grid 0 (succ y) h
  else if y = Array.length grid then
    h
  else
    let h = h ^ String.make 1 grid.(y).(x) in
    hash_grid grid (succ x) y h

let spin grid =
  tiltn grid;
  tiltw grid;
  tilts grid;
  tilte grid

let rec calculate_load grid x y sum =
  if x = Array.length grid.(0) then
    calculate_load grid 0 (succ y) sum
  else if y = Array.length grid then
    sum
  else
    let sum = if grid.(y).(x) = 'O' then
      sum + (Array.length grid) - y
    else
      sum in
    calculate_load grid (succ x) y sum

let part_1 grid =
  let grid = copy_grid grid in
  tiltn grid;
  calculate_load grid 0 0 0

let part_2 grid =
  let seen_cycles = Hashtbl.create 1024 in
  let loads = Hashtbl.create 1024 in
  let running = ref true in
  let result = ref 0 in
  while !running do
    let cycle_count = Hashtbl.length seen_cycles in
    let hash = hash_grid grid 0 0 "" in
    match Hashtbl.find_opt seen_cycles hash with
    | Some previous_cycle_count -> begin
      result := Hashtbl.find loads (((1000000000 - cycle_count) mod (cycle_count - previous_cycle_count)) + previous_cycle_count);
      running := false
    end
    | None -> begin
      Hashtbl.add seen_cycles hash cycle_count
    end;
    Hashtbl.add loads cycle_count (calculate_load grid 0 0 0);
    spin grid
  done;
  !result

let () =
  let ic = open_in "input" in
  let grid = Array.of_list (read_grid ic) in
  close_in ic;
  print_int (part_1 grid);
  print_endline "";
  print_int (part_2 grid);
  print_endline "";
