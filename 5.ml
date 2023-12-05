let rec read_lines file =
  try
    let line = input_line file in
    line :: read_lines file
  with End_of_file ->
    []

let parse_data lines =
  let seeds = List.map int_of_string (List.tl (String.split_on_char ' ' (List.hd lines))) in
  let seed_to_soil   = ref [] in
  let soil_to_fert   = ref [] in
  let fert_to_water  = ref [] in
  let water_to_light = ref [] in
  let light_to_temp  = ref [] in
  let temp_to_humid  = ref [] in
  let humid_to_loc   = ref [] in
  let rec parse_map r l =
    match l with
    | [] -> ()
    | (h :: t) ->
      if String.equal h "" then
        parse_map r t
      else if String.ends_with ~suffix:"map:" h then
        match h with
        | "seed-to-soil map:"            -> parse_map seed_to_soil   t
        | "soil-to-fertilizer map:"      -> parse_map soil_to_fert   t
        | "fertilizer-to-water map:"     -> parse_map fert_to_water  t
        | "water-to-light map:"          -> parse_map water_to_light t
        | "light-to-temperature map:"    -> parse_map light_to_temp  t
        | "temperature-to-humidity map:" -> parse_map temp_to_humid  t
        | "humidity-to-location map:"    -> parse_map humid_to_loc   t
        | _ -> failwith "invalid map"
      else begin
        begin match String.split_on_char ' ' h with
          | a :: b :: c :: [] ->
            r := (int_of_string a, int_of_string b, int_of_string c) :: !r
          | _ -> failwith ""
        end;
        parse_map r t
      end
      in
  parse_map (ref []) (List.tl lines);
  (
    seeds,
    [
      !seed_to_soil;
      !soil_to_fert;
      !fert_to_water;
      !water_to_light;
      !light_to_temp;
      !temp_to_humid;
      !humid_to_loc
    ]
  )

let do_mapping_one v (d_start, s_start, l) =
  if (v >= s_start) && (v < (s_start + l)) then
    Some (d_start + (v - s_start))
  else
    None

let do_mapping v l =
  match List.find_map (do_mapping_one v) l with
  | Some h -> h
  | None   -> v

let rec find_min l =
  match l with
  | h :: [] -> h
  | h :: t  -> let v = find_min t in min v h
  | []      -> failwith ""

let part_1 seeds mappings =
  find_min (List.map (fun s -> List.fold_left do_mapping s mappings) seeds)

let split_range_on_mapping (d_start, s_start, l) (r_start, r_end) =
  let s_end = s_start + l - 1 in
  if (r_end < s_start) || (r_start > s_end) then
    (* easy case - no overlap *)
    [(r_start, r_end)]
  else begin
    (* there is some overlap. *)
    let m_start = max s_start r_start in
    let m_end = min s_end r_end in
    let result = ref [(m_start, m_end)] in
    if m_start > r_start then
      result := (r_start, m_start - 1) :: !result;
    if m_end < r_end then
      result := (m_end + 1, r_end) :: !result;
    !result
  end

let rec iter_mappings ranges mappings =
  match mappings with
  | mapping :: t ->
    let ranges = List.concat_map (split_range_on_mapping mapping) ranges in
    iter_mappings ranges t
  | [] -> ranges

let split_ranges_on_mappings ranges mappings =
  let ranges = iter_mappings ranges mappings in
  let iter_final range =
    let (r_start, r_end) = range in
    let mapped = do_mapping r_start mappings in
    (mapped, (mapped - r_start) + r_end) in
  List.map iter_final ranges

let split_ranges ranges ll =
  List.fold_left split_ranges_on_mappings ranges ll

let create_seed_ranges seeds =
  let a = ref None in
  let result = ref [] in
  let iter seed =
    a := match !a with
    | Some v -> begin
      result := (v, v + seed - 1) :: !result; None
    end
    | None -> Some seed in
  List.iter iter seeds;
  !result

let part_2 seeds mappings =
  let ranges = create_seed_ranges seeds in
  let ranges = split_ranges ranges mappings in
  find_min (List.map (fun (a, _) -> a) ranges)

let () =
  let ic = open_in "input" in
  let lines = read_lines ic in
  close_in ic;
  let (seeds, mappings) = parse_data lines in
  print_int (part_1 seeds mappings);
  print_newline ();
  print_int (part_2 seeds mappings);
  print_newline ();
