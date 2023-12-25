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

let get_edges graph =
  let result = ref [] in
  let iterator i v =
    let iterator2 j =
      if i < j then
        result := (i, j) :: !result in
    List.iter iterator2 v in
  Array.iteri iterator graph;
  Array.of_list !result

let swap_remove array num_edges i =
  array.(i) <- array.(!num_edges - 1);
  num_edges := !num_edges - 1

let contract graph =
  let running = ref true in
  let result = ref 0 in
  let orig_edges = get_edges graph in
  while !running do
    let num_vertices = ref (Array.length graph) in
    let vertex_weights = Array.make !num_vertices 1 in
    let edges = Array.copy orig_edges in
    let num_edges = ref (Array.length edges) in

    while !num_vertices > 2 do
      let edge_index = Random.int !num_edges in
      let edge = edges.(edge_index) in
      swap_remove edges num_edges edge_index;
      (* anything that points to second element should now point to first element *)
      (* TODO speed up by not going through every edge here? *)
      let a, b = edge in
      vertex_weights.(a) <- vertex_weights.(a) + vertex_weights.(b);
      let i = ref 0 in
      while !i < !num_edges do
        let c, d = edges.(!i) in
        let c = if c = b then a else c in
        let d = if d = b then a else d in
        if c = d then begin
          swap_remove edges num_edges !i;
        end else begin
          edges.(!i) <- (c, d);
          i := !i + 1;
        end;
      done;
      num_vertices := !num_vertices - 1;
    done;
    if !num_edges = 3 then begin
      running := false;
      let a, b = edges.(0) in
      let a = vertex_weights.(a) in
      let b = vertex_weights.(b) in
      result := a * b;
    end
  done;
  print_int !result;
  print_endline ""

let () =
  let ic = open_in "input" in
  let graph = Hashtbl.create 2048 in
  read_lines ic graph;
  close_in ic;
  let graph = optimize_graph graph in
  contract graph

