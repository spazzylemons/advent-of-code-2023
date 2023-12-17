type vector = int * int

type state = {
  pos      : vector;
  dir      : vector;
  straight : int;
}

let empty_state : state = {
  pos      = (0, 0);
  dir      = (0, 0);
  straight = 0;
}

type pqueue_entry = {
  cost  : int;
  state : state;
}

type pqueue = {
  array : pqueue_entry Array.t;
  size  : int ref;
  tbl   : (state, unit) Hashtbl.t
}

let max_pqueue_size = 65536

let in_bounds grid pos =
  let x, y = pos in
  x >= 0 && x < Array.length grid.(0) && y >= 0 && y < Array.length grid

let manhattan_distance p1 p2 =
  let x1, y1 = p1 in
  let x2, y2 = p2 in
  (abs (x1 - x2)) + (abs (y1 - y2))

let new_priority_queue () : pqueue =
  {
    array = Array.make max_pqueue_size {cost = 0; state = empty_state};
    size  = ref (-1);
    tbl  =  Hashtbl.create max_pqueue_size;
  }

let parent i = (i - 1) lsr 1

let left_child i = (i lsl 1) + 1

let right_child i = (i lsl 1) + 2

let swap array a b =
  let t = array.(a) in
  array.(a) <- array.(b);
  array.(b) <- t

let shift_up queue i =
  let i = ref i in
  while !i > 0 && queue.array.(parent !i).cost > queue.array.(!i).cost do
    swap queue.array (parent !i) !i;
    i := parent !i
  done

let rec shift_down queue i =
  let max_index = i in
  let l = left_child i in
  let max_index = if l <= !(queue.size) && queue.array.(l).cost < queue.array.(max_index).cost then l else max_index in
  let l = right_child i in
  let max_index = if l <= !(queue.size) && queue.array.(l).cost < queue.array.(max_index).cost then l else max_index in
  if i != max_index then begin
    swap queue.array i max_index;
    shift_down queue max_index
  end

let insert_queue queue k v =
  if Option.is_none (Hashtbl.find_opt queue.tbl k) then begin
    Hashtbl.add queue.tbl k ();
    queue.size := succ !(queue.size);
    queue.array.(!(queue.size)) <- {cost = v; state = k};
    shift_up queue !(queue.size)
  end

let extract_queue queue =
  let result = queue.array.(0).state in
  queue.array.(0) <- queue.array.(!(queue.size));
  queue.size := !(queue.size) - 1;
  shift_down queue 0;
  Hashtbl.remove queue.tbl result;
  result

let part1_goal_verifier goal state =
  state.pos = goal

let part2_goal_verifier goal state =
  state.pos = goal && state.straight > 2

let add_pos (pos : vector) (dir : vector) : vector =
  let x, y = pos in
  let dx, dy = dir in
  (x + dx, y + dy)

let turnl ((dx, dy) : vector) : vector = (-dy, dx)

let turnr ((dx, dy) : vector) : vector = (dy, -dx)

let try_move grid neighbors pos dir straight =
  let pos = add_pos pos dir in
  if in_bounds grid pos then
    neighbors := {pos = pos; dir = dir; straight = straight} :: !neighbors

let part1_neighbor_finder grid state =
  let neighbors = ref [] in
  if state.straight < 2 then
    try_move grid neighbors state.pos state.dir (state.straight + 1);
  try_move grid neighbors state.pos (turnl state.dir) 0;
  try_move grid neighbors state.pos (turnr state.dir) 0;
  !neighbors

let part2_neighbor_finder grid state =
  let neighbors = ref [] in
  if state.straight < 9 then
    try_move grid neighbors state.pos state.dir (state.straight + 1);
  if state.straight > 2 then begin
    try_move grid neighbors state.pos (turnl state.dir) 0;
    try_move grid neighbors state.pos (turnr state.dir) 0
  end;
  !neighbors

let astar grid goal_verifier neighbor_finder =
  let goal : vector = (Array.length grid.(0) - 1, Array.length grid - 1) in
  let goal_verifier = goal_verifier goal in
  let heuristic state =
    let x, y = state.pos in
    let gx, gy = goal in
    (abs (gx - x)) + (abs (gy - y)) in
  let openset = new_priority_queue () in
  let gscore = Hashtbl.create 256 in
  let start = {pos = (0, 0); dir = (1, 0); straight = 0} in
  insert_queue openset start (heuristic start);
  Hashtbl.add gscore start 0;
  let start = {pos = (0, 0); dir = (0, 1); straight = 0} in
  insert_queue openset start (heuristic start);
  Hashtbl.add gscore start 0;
  let result = ref 0 in
  let running = ref true in
  while !running do
    let current = extract_queue openset in
    let current_gscore = Hashtbl.find gscore current in
    if goal_verifier current then begin
      running := false;
      result := current_gscore
    end else begin
      let neighbors = neighbor_finder grid current in
      let iterator neighbor =
        let (x, y) = neighbor.pos in
        let tentative = current_gscore + grid.(y).(x) in
        let should_update = match Hashtbl.find_opt gscore neighbor with
        | Some v -> tentative < v
        | None -> true in
        if should_update then begin
          Hashtbl.replace gscore neighbor tentative;
          insert_queue openset neighbor (tentative + heuristic neighbor);
        end in
      List.iter iterator neighbors
      end
  done;
  !result

let rec read_grid ic =
  try
    let line = input_line ic in
    let line = Array.of_seq (Seq.map (fun a -> int_of_char a - 0x30) (String.to_seq line)) in
    line :: read_grid ic
  with End_of_file ->
    []

let () =
  let ic = open_in "input" in
  let grid = Array.of_list (read_grid ic) in
  close_in ic;
  print_int (astar grid part1_goal_verifier part1_neighbor_finder);
  print_endline "";
  print_int (astar grid part2_goal_verifier part2_neighbor_finder);
  print_endline ""
