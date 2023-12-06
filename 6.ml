let read_input_line ic =
  let line = input_line ic in
  List.filter_map int_of_string_opt (List.tl (String.split_on_char ' ' line))

let merge_times t =
  int_of_string (String.concat "" (List.map string_of_int t))

let find_number_of_winning_times (time, dist) =
  (* Use quadratic formula to find start and end of winning range *)
  (* Get square root of discriminant *)
  let d = sqrt (float_of_int (time * time - 4 * dist)) in
  (* Get roots *)
  let r1 = ((float_of_int time) +. d) *. 0.5 in
  let r2 = ((float_of_int time) -. d) *. 0.5 in
  (* Calculate integer values of start and end *)
  let t1 = if r1 = (floor r1) then (int_of_float r1) - 1 else (int_of_float r1) in
  let t2 = if r2 = (floor r2) then (int_of_float r2) + 1 else (int_of_float (ceil r2)) in
  (* Measure range *)
  t1 - t2 + 1

let part_1 times dists =
  let entries = List.combine times dists in
  List.fold_left ( * ) 1 (List.map find_number_of_winning_times entries)

let part_2 times dists =
  let time = merge_times times in
  let dist = merge_times dists in
  find_number_of_winning_times (time, dist)

let () =
  let ic = open_in "input" in
  let times = read_input_line ic in
  let dists = read_input_line ic in
  close_in ic;
  print_int (part_1 times dists);
  print_endline "";
  print_int (part_2 times dists);
  print_endline ""
