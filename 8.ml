let map_node_to_next dirs graph index node =
  let (l, r) = Hashtbl.find graph node in
  match String.get dirs index with
  | 'L' -> l
  | 'R' -> r
  | _ -> failwith ""

let rec solve_part_1 dirs graph index node =
  if node = "ZZZ" then
    0
  else
    let next_index = (index + 1) mod (String.length dirs) in
    1 + solve_part_1 dirs graph next_index (map_node_to_next dirs graph index node)

let find_ghost_sequence dirs graph node =
  let node = ref node in
  let seen = Hashtbl.create 16384 in
  let index = ref 0 in
  let searching = ref true in
  let result = ref 0 in
  let sequence_index = ref 0 in
  while !searching do
    (* check if we've seen this position before, and if so, where? *)
    match Hashtbl.find_opt seen !node with
    | Some last_seen -> begin
      result := !sequence_index - last_seen;
      searching := false
    end
    | None -> begin
      (* is this an end node? *)
      if String.ends_with ~suffix: "Z" !node then
        (* new entry to sequence *)
        Hashtbl.add seen !node !sequence_index;
      sequence_index := !sequence_index + 1;
      (* where to next? *)
      node := map_node_to_next dirs graph !index !node;
      index := (!index + 1) mod (String.length dirs)
    end
  done;
  (* return period *)
  !result

let rec calc_gcd a b =
  if b = 0 then a else calc_gcd b (a mod b)

let part_1 dirs graph =
  print_int (solve_part_1 dirs graph 0 "AAA");
  print_endline ""

let part_2 dirs graph =
  let nodes = ref [] in
  let graph_iter node _ =
    if String.ends_with ~suffix: "A" node then
      nodes := node :: !nodes in
  Hashtbl.iter graph_iter graph;
  let ghosts = List.map (find_ghost_sequence dirs graph) !nodes in
  let gcd = ref 0 in
  let iter1 i1 g1 =
    let iter2 i2 g2 =
      if i1 < i2 then
        gcd := max !gcd (calc_gcd g1 g2) in
    List.iteri iter2 ghosts in
  List.iteri iter1 ghosts;
  let gcd = !gcd in
  (* no fathomable reason why LCM just works, but i'll take it. *)
  print_int (List.fold_left (fun a b -> a * (b / gcd)) gcd ghosts);
  print_endline ""

let rec parse_graph ic graph =
  try
    let line = input_line ic in
    begin
      match String.split_on_char ' ' line with
      | node :: _ :: l :: r :: [] ->
        let l = String.sub l 1 3 in
        let r = String.sub r 0 3 in
        Hashtbl.add graph node (l, r)
      | _ -> failwith ""
    end;
    parse_graph ic graph
  with End_of_file ->
    ()

let () =
  let ic = open_in "input" in
  let dirs = input_line ic in
  let _ = input_line ic in
  let graph = Hashtbl.create 128 in
  parse_graph ic graph;
  close_in ic;
  part_1 dirs graph;
  part_2 dirs graph
