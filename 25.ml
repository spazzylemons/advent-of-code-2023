(* find this by using graphviz neato *)
let found_pairs = [
  (1198, 229);
  (1207, 485);
  (1336, 156);
]

let add_edge graph a b =
  let edges =
    match Hashtbl.find_opt graph a with
    | None -> []
    | Some v -> v in
  let edges = b :: edges in
  Hashtbl.replace graph a edges

let rec read_lines ic graph =
  try
    let line = input_line ic in
    match String.split_on_char ':' line with
    | a :: b :: [] ->
      let iterator c =
        add_edge graph a c;
        add_edge graph c a in
      List.iter iterator (String.split_on_char ' ' (String.sub b 1 (String.length b - 1)));
      read_lines ic graph
    | _ -> failwith ""
  with End_of_file ->
    ()

let optimize_graph graph =
  let ids = Hashtbl.create 2048 in
  let alt = Hashtbl.create 2048 in
  let iterator k _ =
    let i = Hashtbl.length ids in
    Hashtbl.replace ids k i;
    Hashtbl.replace alt i k in
  Hashtbl.iter iterator graph;
  let init i =
    Hashtbl.find graph (Hashtbl.find alt i)
    |> List.map (Hashtbl.find ids) in
  Array.init (Hashtbl.length ids) init

let draw_graph graph =
  let oc = open_out "graph.dot" in
  output_string oc "graph {\n";
  let iterator i n =
    let iterator2 m = 
      if i < m then
        Printf.fprintf oc "    \"%d\" -- \"%d\"\n" i m in
    List.iter iterator2 n in
  Array.iteri iterator graph;
  output_string oc "}\n";
  flush oc;
  close_out oc

let rec list_remove a xs =
  match xs with
  | x :: xs ->
    if x = a then xs else x :: list_remove a xs
  | [] -> []

let rec remove_pairs graph pairs =
  match pairs with
  | (a, b) :: pairs ->
    graph.(a) <- list_remove b graph.(a);
    graph.(b) <- list_remove a graph.(b);
    remove_pairs graph pairs
  | [] -> ()

let rec find_first_reachable i seen =
  if i = Array.length seen then
    None
  else if not seen.(i) then
    Some i
  else find_first_reachable (i + 1) seen

let rec find_disjoint_size_rec grapn seen node =
  if seen.(node) then
    0
  else begin
    seen.(node) <- true;
    grapn.(node)
      |> List.filter (fun i -> not seen.(i))
      |> List.map (find_disjoint_size_rec grapn seen)
      |> List.fold_left (+) 0
      |> succ
  end
  

let find_disjoint_size graph seen =
  let start = find_first_reachable 0 seen |> Option.get in
  find_disjoint_size_rec graph seen start

let solve_graph graph =
  let seen = Array.make (Array.length graph) false in
  let a = find_disjoint_size graph seen in
  let b = find_disjoint_size graph seen in
  print_int (a * b);
  print_endline ""

let () =
  let ic = open_in "input" in
  let graph = Hashtbl.create 2048 in
  read_lines ic graph;
  close_in ic;
  let graph = optimize_graph graph in
  remove_pairs graph found_pairs;
  solve_graph graph
  (* draw_graph graph *)
