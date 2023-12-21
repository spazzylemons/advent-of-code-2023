let rec read_grid ic =
  try
    let line = input_line ic in
    Array.of_seq (String.to_seq line) :: read_grid ic
  with End_of_file ->
    []

let try_move grid x y =
  x >= 0 && x < Array.length grid.(0) &&
  y >= 0 && y < Array.length grid &&
  not (grid.(y).(x) = '#')

let run_steps grid (x, y, v) =
  let positions = Hashtbl.create 16384 in
  let reachable = Hashtbl.create 16384 in
  if v > 0 then
    Hashtbl.add positions (x, y) v;
  while Hashtbl.length positions > 0 do
    let old_positions = Hashtbl.copy positions in
    Hashtbl.clear positions;
    let iterator (x, y) v =
      if v > 1 then begin
        let v = v - 1 in
        if try_move grid (x - 1) y then
          Hashtbl.replace positions (x - 1, y) v;
        if try_move grid (x + 1) y then
          Hashtbl.replace positions (x + 1, y) v;
        if try_move grid x (y - 1) then
          Hashtbl.replace positions (x, y - 1) v;
        if try_move grid x (y + 1) then
          Hashtbl.replace positions (x, y + 1) v
      end else
        Hashtbl.replace reachable (x, y) () in
    Hashtbl.iter iterator old_positions
  done;
  Hashtbl.length reachable

let run_big_steps grid step_count =
  let w = Array.length grid in
  let t = w / 2 in
  let sd = (step_count mod w) in
  let id = ((step_count + t + 1) mod w) in
  let a = run_steps grid (w - 1, w - 1, sd    ) in
  let b = run_steps grid (w - 1, w - 1, w + sd) in
  let c = run_steps grid (t,     w - 1, w + id) in
  let d = run_steps grid (t,     w - 1, id    ) in
  let e = run_steps grid (0,     w - 1, sd    ) in
  let f = run_steps grid (0,     w - 1, w + sd) in
  let g = run_steps grid (0,     t,     w + id) in
  let h = run_steps grid (0,     t,     id    ) in
  let i = run_steps grid (0,     0,     sd    ) in
  let j = run_steps grid (0,     0,     w + sd) in
  let k = run_steps grid (t,     0,     w + id) in
  let l = run_steps grid (t,     0,     id    ) in
  let m = run_steps grid (w - 1, 0,     sd    ) in
  let n = run_steps grid (w - 1, 0,     w + sd) in
  let o = run_steps grid (w - 1, t,     w + id) in
  let p = run_steps grid (w - 1, t,     id    ) in
  let r = run_steps grid (t,     t,     999   ) in
  let q = run_steps grid (t,     t,     1000  ) in
  let xd = (step_count) / w in
  let yd = ((xd - 1) / 2) + 1 in
  let zd = (xd / 2) + 1 in
  let yd = 4 * yd * yd - 4 * yd + 1 in
  let zd = 4 * zd * zd - 8 * zd + 4 in
  let yd, zd = if (xd mod 2) = 1 then
    let yd = yd - if (w + id) > w + t then 4 else 0 in
    let zd = zd in
    yd, zd
  else
    let yd = yd in
    let zd = zd - if (w + id) > w + t then 4 else 0 in
    yd, zd in
  let result =
    (xd * (a + e + i + m)) +
    ((xd - 1) * (b + f + j + n)) +
    c + d + g + h + k + l + o + p +
    (yd * q) +
    (zd * r) in
  result

let () =
  let ic = open_in "input" in
  let grid = Array.of_list (read_grid ic) in
  let w = (Array.length grid) / 2 in
  close_in ic;
  (* part 1 *)
  print_int (run_steps grid (w, w, 65));
  print_endline "";
  (* part 2 *)
  print_int (run_big_steps grid 26501365);
  print_endline "";
