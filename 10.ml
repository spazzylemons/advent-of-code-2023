let rec read_lines ic =
  try
    let line = input_line ic in
    (Array.of_seq (String.to_seq line)) :: read_lines ic
  with End_of_file ->
    []

let rec find_starting_position maze x y =
  if maze.(y).(x) = 'S' then
    (x, y)
  else
    let (x, y) = if x = ((Array.length maze.(0)) - 1) then (0, y + 1) else (x + 1, y) in
    find_starting_position maze x y

let can_check_l maze x = x > 0

let can_check_r maze x = x < Array.length maze.(0) - 1

let can_check_u maze y = y > 0

let can_check_d maze y = y < Array.length maze - 1

let can_move_l c = c = '-' || c = '7' || c = 'J'

let can_move_r c = c = '-' || c = 'L' || c = 'F'

let can_move_u c = c = '|' || c = 'L' || c = 'J'

let can_move_d c = c = '|' || c = '7' || c = 'F'

let is_connected_l maze x y =
  (x > 0) && (can_move_r maze.(y).(x - 1))

let is_connected_r maze x y =
  (x < (Array.length maze.(0) - 1)) && (can_move_l maze.(y).(x + 1))

let is_connected_u maze x y =
  (y > 0) && (can_move_d maze.(y - 1).(x))

let is_connected_d maze x y =
  (y < (Array.length maze - 1)) && (can_move_u maze.(y + 1).(x))

let replace_starting_tile maze x y =
  let l = is_connected_l maze x y in
  let r = is_connected_r maze x y in
  let u = is_connected_u maze x y in
  let d = is_connected_d maze x y in
  maze.(y).(x) <-
    match (l, r, u, d) with
    | (true, true, false, false) -> '-'
    | (false, false, true, true) -> '|'
    | (true, false, true, false) -> 'J'
    | (true, false, false, true) -> '7'
    | (false, true, true, false) -> 'L'
    | (false, true, false, true) -> 'F'
    | _ -> failwith ""

let copy_maze maze =
  Array.map Array.copy maze

let measure_distance maze x y =
  let stack = Queue.create () in
  Queue.push (x, y, 0) stack;
  let result = ref 0 in
  let tiles = Hashtbl.create 4096 in
  while not (Queue.is_empty stack) do
    let (x, y, distance) = Queue.pop stack in
    result := max !result distance;
    let distance = 1 + distance in
    let c = maze.(y).(x) in
    let l = (can_move_l c) && (is_connected_l maze x y) in
    let r = (can_move_r c) && (is_connected_r maze x y) in
    let u = (can_move_u c) && (is_connected_u maze x y) in
    let d = (can_move_d c) && (is_connected_d maze x y) in
    maze.(y).(x) <- '.';
    Hashtbl.add tiles (x, y) ();
    if l then Queue.push (x - 1, y, distance) stack;
    if r then Queue.push (x + 1, y, distance) stack;
    if u then Queue.push (x, y - 1, distance) stack;
    if d then Queue.push (x, y + 1, distance) stack;
  done;
  (!result, tiles)

let cw (x, y) = (-y, x)

let ccw (x, y) = (y, -x)

let can_check maze x y =
  (x >= 0) && (x < Array.length maze.(0)) && (y >= 0) && (y < Array.length maze)

let fill_inner maze x y tiles =
  (* Queue of filled tiles. *)
  let queue = Queue.create () in
  (* Choose initial direction based on start tile. *)
  let forward = ref (
    let c = maze.(y).(x) in
    if (can_move_l c) && (is_connected_l maze x y) then
      (-1, 0)
    else if (can_move_r c) && (is_connected_r maze x y) then
      (1, 0)
    else if (can_move_u c) && (is_connected_u maze x y) then
      (0, -1)
    else
      (0, 1)
    ) in
  (* Save starting position. *)
  let sx = x in
  let sy = y in
  (* Move until we hit the starting position again. *)
  let running = ref true in
  let x = ref x in
  let y = ref y in
  let try_fill add c =
    if can_check maze (!x + fst add) (!y + snd add) && Option.is_none (Hashtbl.find_opt tiles (!x + fst add, !y + snd add)) then begin
      Hashtbl.add tiles (!x + fst add, !y + snd add) ();
      Queue.push (!x + fst add, !y + snd add, c) queue
    end in
  let fill_surroundings () =
    (* Get rotations. *)
    let left = ccw !forward in
    let right = cw !forward in
    (* Add left and right tiles, if ground tiles, to fill queue. *)
    try_fill left 'A';
    try_fill right 'B'; in
  let rotate_left () =
    forward := ccw !forward;
    fill_surroundings (); in
  let rotate_right () =
    forward := cw !forward;
    fill_surroundings (); in
  while !running do
    (* Fill surroundings of current tile. *)
    fill_surroundings ();
    (* If we need to rotate, then do so. *)
    let c = maze.(!y).(!x) in
    begin match !forward with
    | (1, 0) ->
        if c = 'J' then
          rotate_left ()
        else if c = '7' then
          rotate_right ()
    | (-1, 0) -> 
      if c = 'F' then
        rotate_left ()
      else if c = 'L' then
        rotate_right ()
    | (0, -1) ->
      if c = '7' then
        rotate_left ()
      else if c = 'F' then
        rotate_right ()
    | (0, 1) ->
      if c = 'L' then
        rotate_left ()
      else if c = 'J' then
        rotate_right ()
    | _ -> failwith ""
    end;
    (* Move to next tile. *)
    x := !x + fst !forward;
    y := !y + snd !forward;
    (* Stop if we hit starting position. *)
    if !x = sx && !y = sy then
      running := false
  done;
  (* Now flood fill the rest. *)
  while not (Queue.is_empty queue) do
    let (x, y, c) = Queue.pop queue in
    maze.(y).(x) <- c;
    if can_check_l maze x && Option.is_none (Hashtbl.find_opt tiles (x - 1, y)) then begin
      Hashtbl.add tiles (x - 1, y) ();
      Queue.push (x - 1, y, c) queue
    end;
    if can_check_r maze x && Option.is_none (Hashtbl.find_opt tiles (x + 1, y)) then begin
      Hashtbl.add tiles (x + 1, y) ();
      Queue.push (x + 1, y, c) queue
    end;
    if can_check_u maze y && Option.is_none (Hashtbl.find_opt tiles (x, y - 1)) then begin
      Hashtbl.add tiles (x, y - 1) ();
      Queue.push (x, y - 1, c) queue
    end;
    if can_check_d maze y && Option.is_none (Hashtbl.find_opt tiles (x, y + 1)) then begin
      Hashtbl.add tiles (x, y + 1) ();
      Queue.push (x, y + 1, c) queue;
    end
  done;
  (* Assume the smaller area is the inner loop.*)
  let a = ref 0 in
  let b = ref 0 in
  for y = 0 to Array.length maze - 1 do
    for x = 0 to Array.length maze.(0) - 1 do
      match maze.(y).(x) with
      | 'A' -> a := succ !a
      | 'B' -> b := succ !b
      | _ -> ()
    done
  done;
  min !a !b

let () =
  let ic = open_in "input" in
  let maze = Array.of_list (read_lines ic) in
  close_in ic;
  let (x, y) = find_starting_position maze 0 0 in
  replace_starting_tile maze x y;
  let (distance, tiles) = measure_distance (copy_maze maze) x y in
  print_int distance;
  print_endline "";
  print_int (fill_inner maze x y tiles);
  print_endline "";
