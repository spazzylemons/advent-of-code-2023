type modstate = 
| Cast
| FF   of bool
| Conj of {
    table  : (string, bool) Hashtbl.t;
    lcount : int ref;
  }

type modl = {
  state   : modstate;
  outputs : string list;
}

type state = (string, modl) Hashtbl.t

type pulse = {
  sender   : string;
  receiver : string;
  value    : bool;
}

let reset_modstate state =
  match state with
  | Cast -> Cast
  | FF _ -> FF false
  | Conj {table; lcount} ->
    let new_table = Hashtbl.create 8 in
    let iterator k _ =
      Hashtbl.add new_table k false in
    Hashtbl.iter iterator table;
    let new_lcount = Hashtbl.length new_table in
    Conj {table = new_table; lcount = ref new_lcount}

let rec parse_outputs str start =
  match String.index_from_opt str start ',' with
  | Some idx ->
    let name = String.sub str start (idx - start) in
    name :: parse_outputs str (idx + 2)
  | None ->
    String.sub str start (String.length str - start) :: []

let rec parse_state ic state =
  try
    let line = input_line ic in
    let space = String.index line ' ' in
    let name = String.sub line 0 space in
    let outputs = parse_outputs line (space + 4) in
    let name, modstate =
      match String.unsafe_get name 0 with
      | '%' ->
        let name = String.sub name 1 (String.length name - 1) in
        let modstate = FF false in
        (name, modstate)
      | '&' ->
        let name = String.sub name 1 (String.length name - 1) in
        let modstate = Conj {table = (Hashtbl.create 8); lcount = ref 0} in
        (name, modstate)
      | _ ->
        (name, Cast)
      in
    Hashtbl.add state name {state = modstate; outputs = outputs};
    parse_state ic state
  with End_of_file ->
    ()

let rec conj_finder state from outputs =
  match outputs with
  | output :: outputs ->
    let modl = Hashtbl.find_opt state output in
    begin
      match modl with
      | Some modl ->
        begin
          match modl.state with
          | Conj {table; lcount} ->
            Hashtbl.add table from false;
            lcount := !lcount + 1;
          | _ -> ()
        end
      | None -> ()
    end;
    conj_finder state from outputs
  | [] -> ()

let connect_conj state =
  let iterator k v =
    conj_finder state k v.outputs in
  Hashtbl.iter iterator state

let rec queue_pulse queue from value outputs =
  match outputs with
  | output :: outputs ->
    Queue.add {sender = from; receiver = output; value = value} queue;
    queue_pulse queue from value outputs;
  | [] -> ()

let receive_pulse state from name value queue =
  let receiver = Hashtbl.find_opt state name in
  match receiver with
  | Some receiver ->
    let new_state =
      match receiver.state with
      | Cast ->
        queue_pulse queue name value receiver.outputs;
        receiver.state
      | FF v ->
        if not value then begin
          queue_pulse queue name (not v) receiver.outputs;
          FF (not v)
        end else
          receiver.state
      | Conj {table; lcount} ->
        let prev = Hashtbl.find table from in
        if not (prev = value) then begin
          Hashtbl.replace table from value;
          if value then
            lcount := !lcount - 1
          else
            lcount := !lcount + 1
        end;
        queue_pulse queue name (not (!lcount = 0)) receiver.outputs;
        receiver.state
      in
    Hashtbl.replace state name {state = new_state; outputs = receiver.outputs}
  | None -> ()

let run_pulses state lo hi =
  let queue = Queue.create () in
  queue_pulse queue "broadcaster" false (Hashtbl.find state "broadcaster").outputs; 
  lo := !lo + 1;
  while not (Queue.is_empty queue) do
    let pulse = Queue.take queue in
    if pulse.value then
      hi := !hi + 1
    else
      lo := !lo + 1;
    receive_pulse state pulse.sender pulse.receiver pulse.value queue;
  done

let rec build_subgraph state subgraph outputs =
  match outputs with
  | output :: outputs ->
    if Option.is_none (Hashtbl.find_opt subgraph output) then begin
      let modl = Hashtbl.find state output in
      if not (modl.outputs = ["rx"]) then begin
        Hashtbl.add subgraph output {state = (reset_modstate modl.state); outputs = modl.outputs};
        build_subgraph state subgraph modl.outputs
      end else begin
        Hashtbl.add subgraph output {state = Cast; outputs = modl.outputs};
      end
    end;
    build_subgraph state subgraph outputs
  | [] -> ()

let rec find_subgraphs state outputs =
  match outputs with
  | output :: outputs ->
    let subgraph = Hashtbl.create 32 in
    let modl = Hashtbl.find state "broadcaster" in
    Hashtbl.add subgraph "broadcaster" {state = (reset_modstate modl.state); outputs = modl.outputs};
    let modl = Hashtbl.find state output in
    build_subgraph state subgraph modl.outputs;
    subgraph :: find_subgraphs state outputs
  | [] -> []

let run_subgraph state =
  let queue = Queue.create () in
  let count = ref 0 in
  let running = ref true in
  while !running do
    count := !count + 1;
    queue_pulse queue "broadcaster" false (Hashtbl.find state "broadcaster").outputs;
    while not (Queue.is_empty queue) do
      let pulse = Queue.take queue in
      if pulse.receiver = "rx" && pulse.value then
        running := false;
      receive_pulse state pulse.sender pulse.receiver pulse.value queue;
    done
  done;
  !count

let rec gcd a b =
  if b = 0 then a else gcd b (a mod b)

let rec calc_gcd_list a =
  match a with
  | a :: b :: t ->
    let g = gcd a b in
    calc_gcd_list (g :: t)
  | a :: [] ->
    a
  | [] -> 1

let lcm a =
  let g = calc_gcd_list a in
  List.fold_left (fun a b -> a * (b / g)) g a

let part_2 state =
  (find_subgraphs state (Hashtbl.find state "broadcaster").outputs)
  |> List.map run_subgraph
  |> lcm

let part_1 state =
  let lo = ref 0 in
  let hi = ref 0 in
  for i = 1 to 1000 do
    run_pulses state lo hi
  done;
  !lo * !hi

let load_state filename =
  let state = Hashtbl.create 128 in
  let ic = open_in filename in
  parse_state ic state;
  close_in ic;
  connect_conj state;
  state

let () =
  let state = load_state "input" in
  print_int (part_1 state);
  print_endline "";
  print_int (part_2 state);
  print_endline ""
