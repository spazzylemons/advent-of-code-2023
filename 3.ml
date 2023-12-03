let to_digit c =
  if (c >= '0') && (c <= '9') then
    Some ((int_of_char c) - 0x30)
  else
    None

let parse_numbers numbers symbols y s =
  let t = ref None in
  for i = 0 to String.length s do
    let h = if i == String.length s then '.' else String.unsafe_get s i in
    match to_digit h with
    | Some c -> begin
      t := Some (match !t with
      | Some (p, v) -> (p, ((v * 10) + c))
      | None -> (i, c))
    end
    | None -> begin
      if h != '.' then
        Hashtbl.add symbols (i, y) h;
      match !t with
      | Some (j, v) -> begin
        for x = j to i - 1 do
          Hashtbl.add numbers (x, y) (v, j, y)
        done;
        t := None
      end
      | None -> ()
    end
  done

let rec load_table numbers symbols ic i =
  try
    let line = input_line ic in
    parse_numbers numbers symbols i line;
    load_table numbers symbols ic (i + 1)
  with End_of_file ->
    ()

let part_1 numbers symbols =
  let seen_number_coords = Hashtbl.create 128 in
  let sum = ref 0 in
  let symbol_iterator (x, y) _ =
    for px = -1 to 1 do
      for py = -1 to 1 do
        match Hashtbl.find_opt numbers (x + px, y + py) with
        | Some (number, x, y) ->
          if Option.is_none (Hashtbl.find_opt seen_number_coords (x, y)) then begin
            Hashtbl.add seen_number_coords (x, y) ();
            sum := !sum + number
          end;
        | None -> ()
      done
    done in
  Hashtbl.iter symbol_iterator symbols;
  !sum

let part_2 numbers symbols =
  let sum = ref 0 in
  let symbol_iterator (x, y) s =
    if s == '*' then
      let gear_numbers = ref [] in
      let seen_number_coords = Hashtbl.create 8 in
      for px = -1 to 1 do
        for py = -1 to 1 do
          match Hashtbl.find_opt numbers (x + px, y + py) with
          | Some (number, x, y) ->
            if Option.is_none (Hashtbl.find_opt seen_number_coords (x, y)) then begin
              Hashtbl.add seen_number_coords (x, y) ();
              gear_numbers := number :: !gear_numbers
            end;
          | None -> ()
        done
      done;
      match !gear_numbers with
      | g1 :: g2 :: [] ->
        sum := !sum + (g1 * g2)
      | _ -> ()
      in
  Hashtbl.iter symbol_iterator symbols;
  !sum

let () =
  let ic = open_in "input" in
  let numbers = Hashtbl.create 128 in
  let symbols = Hashtbl.create 128 in
  begin try
    load_table numbers symbols ic 0;
    close_in ic
  with e ->
    close_in_noerr ic;
    raise e
  end;
  print_int (part_1 numbers symbols);
  print_endline "";
  print_int (part_2 numbers symbols);
  print_endline "";
