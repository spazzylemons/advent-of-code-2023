let rec read_grid ic =
  try
    let line = input_line ic in
    (Array.of_seq (String.to_seq line)) :: read_grid ic
  with End_of_file ->
    []

let can_move grid seen steep pos delta =
  let x, y = pos in
  let dx, dy = delta in
  let x = x + dx in
  let y = y + dy in
  if
    x >= 0 &&
    x < Array.length grid &&
    y >= 0 &&
    y < Array.length grid &&
    not seen.(y).(x)
  then
    match grid.(y).(x) with
    | '.' -> true
    | '#' -> false
    | '^' -> if steep then delta = (0, -1) else true
    | 'v' -> if steep then delta = (0, 1) else true
    | '<' -> if steep then delta = (-1, 0) else true
    | '>' -> if steep then delta = (1, 0) else true
    | _ -> failwith ""
  else false

let try_move grid seen steep pos delta neighbors =
  if can_move grid seen steep pos delta then
    let x, y = pos in
    let dx, dy = delta in
    let x = x + dx in
    let y = y + dy in
    (x, y) :: neighbors
  else
    neighbors

let copy2 array =
  Array.init (Array.length array) (fun i -> Array.copy array.(i))

let rec find_longest_path grid pos seen accum steep =
  let x, y = pos in
  if y = (Array.length grid) - 1 then begin
    Some accum
  end else begin
    let accum = accum + 1 in
    seen.(y).(x) <- true;
    let neighbors = [] in
    let neighbors = try_move grid seen steep pos (-1, 0) neighbors in
    let neighbors = try_move grid seen steep pos (1, 0) neighbors in
    let neighbors = try_move grid seen steep pos (0, -1) neighbors in
    let neighbors = try_move grid seen steep pos (0, 1) neighbors in
    search_neighbors grid neighbors seen accum steep
  end
and search_neighbors grid neighbors seen accum steep =
  match neighbors with
  | a :: b :: t ->
    let copy = copy2 seen in
    let accuma = find_longest_path grid a copy accum steep in
    let accumb = search_neighbors grid (b :: t) seen accum steep in
    begin
      match accuma, accumb with
      | None, None -> None
      | Some a, None -> Some a
      | None, Some b -> Some b
      | Some a, Some b -> Some (max a b)
    end
  | a :: [] -> find_longest_path grid a seen accum steep
  | [] -> None

let can_move_2 grid pos =
  let x, y = pos in
  x >= 0 &&
  x < Array.length grid &&
  y >= 0 &&
  y < Array.length grid &&
  not (grid.(y).(x) = '#')

let try_move_2 grid pos neighbors =
  if can_move_2 grid pos then
    let x, y = pos in
    neighbors := (x, y, 1) :: !neighbors

let rec build_graph grid x y graph =
  if x = Array.length grid then begin
    let y = y + 1 in
    if y < Array.length grid then
      build_graph grid 0 y graph
  end else begin
    if not (grid.(y).(x) = '#') then begin
      let list =
        match Hashtbl.find_opt graph (x, y) with
        | Some list -> list
        | None ->
          let list = ref [] in
          Hashtbl.add graph (x, y) list;
          list in
      try_move_2 grid (x - 1, y) list;
      try_move_2 grid (x + 1, y) list;
      try_move_2 grid (x, y - 1) list;
      try_move_2 grid (x, y + 1) list
    end;
    build_graph grid (x + 1) y graph
  end

let rec list_replace list x1 y1 x2 y2 mw =
  match list with
  | (x, y, w) :: xs ->
    if x = x1 && y = y1 then
      (x2, y2, w + mw) :: xs
    else
      (x, y, w) :: list_replace xs x1 y1 x2 y2 mw
  | [] -> []

let trim_graph goal graph =
  let entries_to_remove = Hashtbl.create 128 in
  let running = ref true in
  while !running do
    (* print_endline "iterate"; *)
    running := false;
    Hashtbl.clear entries_to_remove;
    let iterator (x, y) neighbors =
      (* if we've only got two neighbors, connect them directly *)
      match !neighbors with
      | a :: b :: [] ->
        begin
          let ax, ay, aw = a in
          let bx, by, bw = b in
          let al = Hashtbl.find graph (ax, ay) in
          let bl = Hashtbl.find graph (bx, by) in
          al := list_replace !al x y bx by bw;
          bl := list_replace !bl x y ax ay aw;
          Hashtbl.replace entries_to_remove (x, y) ();
        end
      | _ -> () in
    Hashtbl.iter iterator graph;
    let iterator k _ =
      Hashtbl.remove graph k in
    Hashtbl.iter iterator entries_to_remove
  done;
  let (sx, sy, sw) = List.hd !(Hashtbl.find graph (1, 0)) in
  let (ex, ey, ew) = List.hd !(Hashtbl.find graph goal) in
  let iterator (x, y) neighbors =
    neighbors := list_replace !neighbors sx sy 1 0 sw;
    neighbors := list_replace !neighbors ex ey (fst goal) (snd goal) ew;
    neighbors := List.filter (fun (x2, y2, _) -> not (x = x2 && y = y2)) !neighbors;
    if not ((x, y) = (1, 0) || (x, y) = goal) then begin
      match List.find_opt (fun (x2, y2, _) -> (x2, y2) = (1, 0) || (x2, y2) = goal) !neighbors with
      | Some (x2, y2, w) ->
        let neighbors = Hashtbl.find graph (x2, y2) in
        neighbors := (x, y, w) :: !neighbors
      | None -> ()
    end
  in
  Hashtbl.remove graph (sx, sy);
  Hashtbl.remove graph (ex, ey);
  Hashtbl.iter iterator graph;
  let final_graph = Hashtbl.create 128 in
  let ids = Hashtbl.create 128 in
  Hashtbl.add ids goal 0;
  Hashtbl.add ids (1, 0) 1;
  let iterator k v =
    if Option.is_none (Hashtbl.find_opt ids k) then
      Hashtbl.add ids k (Hashtbl.length ids) in
  Hashtbl.iter iterator graph;
  let iterator k v =
    let neighbors = List.map (fun (x, y, w) -> (Hashtbl.find ids (x, y)), w) !v in
    Hashtbl.add final_graph (Hashtbl.find ids k) neighbors in
  Hashtbl.iter iterator graph;
  final_graph

let create_graph grid =
  let graph = Hashtbl.create 4096 in
  let goal = (Array.length grid - 2, Array.length grid - 1) in
  build_graph grid 0 0 graph;
  trim_graph goal graph

let rec traverse_graph seen graph weight pos =
  if pos = 0 then
    Some weight
  else begin
    Hashtbl.replace seen pos ();
    let neighbors = List.filter
      (fun (p, _) -> Option.is_none (Hashtbl.find_opt seen p))
      (Hashtbl.find graph pos)
      in
    match (search_graph_neighbors seen graph neighbors) with
    | Some v -> Some (weight + v)
    | None -> None
  end
and search_graph_neighbors seen graph neighbors =
  match neighbors with
  | (a, w) :: b :: t ->
    let copy = Hashtbl.copy seen in
    let accuma = traverse_graph copy graph w a in
    let accumb = search_graph_neighbors seen graph (b :: t) in
    begin
      match accuma, accumb with
      | None, None -> None
      | Some a, None -> Some a
      | None, Some b -> Some b
      | Some a, Some b -> Some (max a b)
    end
  | (a, w) :: [] -> traverse_graph seen graph w a
  | [] -> None

let part_1 grid =
  let l = Array.length grid in
  let seen = Array.init l (fun _ -> Array.make l false) in
  let result = Option.get (find_longest_path grid (1, 0) seen 0 true) in
  print_int result;
  print_endline ""

let part_2 grid =
  let graph = create_graph grid in
  let seen = Hashtbl.create 128 in
  let result = traverse_graph seen graph 0 1 in
  print_int (Option.get result);
  print_endline ""

let () =
  let ic = open_in "input" in
  let grid = Array.of_list (read_grid ic) in
  close_in ic;
  part_1 grid;
  part_2 grid
