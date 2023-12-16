let rec read_grid ic =
  try
    let line = input_line ic in
    let line = Array.of_seq (String.to_seq line) in
    line :: read_grid ic
  with End_of_file ->
    []

let move (dx, dy) (x, y) = (dx + x, dy + y)

let rec iter seen covered grid dir pos =
  if Option.is_none (Hashtbl.find_opt seen (dir, pos)) then begin
    let dx, dy = dir in
    let x, y = pos in
    Hashtbl.add seen (dir, pos) ();
    if x >= 0 && x < Array.length grid && y >= 0 && y < Array.length grid then begin
      Hashtbl.replace covered pos ();
      match grid.(y).(x) with
      | '.' ->
        let pos = move dir pos in
        iter seen covered grid dir pos
      | '|' ->
        if dy = 0 then begin
          iter seen covered grid (0, -1) pos;
          iter seen covered grid (0, 1) pos;
        end else
          let pos = move dir pos in
          iter seen covered grid dir pos
      | '-' ->
        if dx = 0 then begin
          iter seen covered grid (-1, 0) pos;
          iter seen covered grid (1, 0) pos;
        end else
          let pos = move dir pos in
          iter seen covered grid dir pos
      | '/' -> begin
        match dir with
        | (-1, 0) ->
            let dir = (0, 1) in
            let pos = move dir pos in
            iter seen covered grid dir pos
        | (0, -1) ->
          let dir = (1, 0) in
          let pos = move dir pos in
          iter seen covered grid dir pos
        | (1, 0) ->
          let dir = (0, -1) in
          let pos = move dir pos in
          iter seen covered grid dir pos
        | (0, 1) ->
          let dir = (-1, 0) in
          let pos = move dir pos in
          iter seen covered grid dir pos
        | _ -> failwith ""
      end
      | '\\' ->  begin
        match dir with
        | (1, 0) ->
            let dir = (0, 1) in
            let pos = move dir pos in
            iter seen covered grid dir pos
        | (0, -1) ->
          let dir = (-1, 0) in
          let pos = move dir pos in
          iter seen covered grid dir pos
        | (-1, 0) ->
          let dir = (0, -1) in
          let pos = move dir pos in
          iter seen covered grid dir pos
        | (0, 1) ->
          let dir = (1, 0) in
          let pos = move dir pos in
          iter seen covered grid dir pos
        | _ -> failwith ""
      end
      | _ -> failwith ""
    end
  end

let () =
  let ic = open_in "input" in
  let grid = Array.of_list (read_grid ic) in
  close_in ic;
  let seen = Hashtbl.create (4 * (Array.length grid) * (Array.length grid)) in
  let covered = Hashtbl.create ((Array.length grid) * (Array.length grid)) in
  iter seen covered grid (1, 0) (0, 0);
  print_int (Hashtbl.length covered);
  print_endline "";
  let max_value = ref 0 in
  for i = 0 to Array.length grid - 1 do
    let seen = Hashtbl.create (4 * (Array.length grid) * (Array.length grid)) in
    let covered = Hashtbl.create ((Array.length grid) * (Array.length grid)) in
    iter seen covered grid (0, 1) (i, 0);
    max_value := max !max_value (Hashtbl.length covered);
    let seen = Hashtbl.create (4 * (Array.length grid) * (Array.length grid)) in
    let covered = Hashtbl.create ((Array.length grid) * (Array.length grid)) in
    iter seen covered grid (1, 0) (0, i);
    max_value := max !max_value (Hashtbl.length covered);
    let seen = Hashtbl.create (4 * (Array.length grid) * (Array.length grid)) in
    let covered = Hashtbl.create ((Array.length grid) * (Array.length grid)) in
    iter seen covered grid (0, -1) (i, Array.length grid - 1);
    max_value := max !max_value (Hashtbl.length covered);
    let seen = Hashtbl.create (4 * (Array.length grid) * (Array.length grid)) in
    let covered = Hashtbl.create ((Array.length grid) * (Array.length grid)) in
    iter seen covered grid (-1, 0) (Array.length grid - 1, i);
    max_value := max !max_value (Hashtbl.length covered);
  done;
  print_int !max_value;
  print_endline "";
