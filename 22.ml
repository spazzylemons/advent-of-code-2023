type vector = {
  x : int;
  y : int;
  z : int;
}

type brick = {
  id : int;
  (* min coordinate of aabb *)
  min : vector;
  (* max coordinate of aabb *)
  max : vector;
  (* list of bricks that are supported by this brick *)
  children : brick list ref;
  (* list of bricks that this brick supports *)
  parents  : brick list ref;
}

let read_vector str =
  match String.split_on_char ',' str with
  | x :: y :: z :: [] ->
    let x = int_of_string x in
    let y = int_of_string y in
    let z = int_of_string z in
    {x = x; y = y; z = z}
  | _ -> failwith ""

let rec read_input id ic =
  try
    let line = input_line ic in
    match String.split_on_char '~' line with
    | a :: b :: [] ->
      let a = read_vector a in
      let b = read_vector b in
      let brick = {id = id; min = a; max = b; children = ref []; parents = ref []} in
      brick :: read_input (id + 1) ic
    | _ -> failwith ""
  with End_of_file ->
    []

let rec find_supporters supporting bricks =
  match bricks with
  | brick :: bricks ->
    if
      brick.min.x <= supporting.max.x &&
      brick.max.x >= supporting.min.x &&
      brick.min.y <= supporting.max.y &&
      brick.max.y >= supporting.min.y
    then begin
      brick.parents := supporting :: !(brick.parents);
      supporting.children := brick :: !(supporting.children);
    end;
    find_supporters supporting bricks
  | [] -> ()

let rec drop_bricks bricks changed =
  match bricks with
  | brick :: bricks ->
    let changed, brick = 
      if (!(brick.parents) = []) && (brick.min.z > 1) then begin
        true,
        {
          brick with
          min = {brick.min with z = brick.min.z - 1};
          max = {brick.max with z = brick.max.z - 1};
        }
      end else
        changed, brick in
    let changed, bricks = drop_bricks bricks changed in
    changed, brick :: bricks
  | [] -> changed, []

let rec map_bricks_to_z_value zmap bricks =
  match bricks with
  | brick :: bricks ->
    brick.children := [];
    brick.parents := [];
    let value =
      match Hashtbl.find_opt zmap brick.min.z with
      | Some v -> v
      | None -> [] in
    let value = brick :: value in
    Hashtbl.replace zmap brick.min.z value;
    map_bricks_to_z_value zmap bricks
  | [] -> ()

let rec build_connections zmap bricks =
  match bricks with
  | brick :: bricks ->
    begin
      match Hashtbl.find_opt zmap (brick.max.z + 1) with
        | Some bricks -> find_supporters brick bricks
        | None -> ()
    end;
    build_connections zmap bricks
  | [] -> ()

let rec settle_bricks bricks =
  let zmap = Hashtbl.create 2048 in
  map_bricks_to_z_value zmap bricks;
  build_connections zmap bricks;
  let changed, bricks = drop_bricks bricks false in
  if changed then
    settle_bricks bricks
  else
    bricks

let rec can_disintigrate supporting bricks =
  match bricks with
  | brick :: bricks ->
    if List.length !(brick.parents) = 1 then
      false
    else
      can_disintigrate supporting bricks
  | [] -> true

let rec part_1 bricks accum =
  match bricks with
  | brick :: bricks ->
    let accum = if can_disintigrate brick !(brick.children) then
      accum + 1
    else accum in
    part_1 bricks accum
  | [] -> accum

let rec will_child_fall fallen parents =
  match parents with
  | parent :: parents ->
    if Option.is_none (Hashtbl.find_opt fallen parent.id) then
      false
    else will_child_fall fallen parents
  | [] -> true

let rec chain_reaction fallen brick =
  (* mark brick as fallen *)
  Hashtbl.replace fallen brick.id ();
  (* check each child *)
  chain_children fallen !(brick.children)
and chain_children fallen children =
  match children with
  | child :: children ->
    let parents = !(child.parents) in
    if not (parents = []) && (will_child_fall fallen parents) then
      chain_reaction fallen child;
    chain_children fallen children
  | [] -> ()

let rec part_2 bricks accum =
  match bricks with
  | brick :: bricks ->
    let fallen = Hashtbl.create 128 in
    chain_reaction fallen brick;
    let accum = accum + Hashtbl.length fallen - 1 in
    part_2 bricks accum
  | [] -> accum

let () =
  let ic = open_in "input" in
  let bricks = read_input 0 ic in
  close_in ic;
  let bricks = settle_bricks bricks in
  print_int (part_1 bricks 0);
  print_endline "";
  print_int (part_2 bricks 0);
  print_endline ""
