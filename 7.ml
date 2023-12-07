let rank_map_1 = "23456789TJQKA";;
let rank_map_2 = "J23456789TQKA";;

let classify_hand joker hand =
  let counts = Hashtbl.create 5 in
  let max_unique_count = ref 0 in
  let joker_count = ref 0 in
  let hand_iter c =
    let count =
      match Hashtbl.find_opt counts c with
      | Some v -> v + 1
      | None -> 1 in
    Hashtbl.replace counts c count;
    let is_joker = c = 'J' in
    if not is_joker || not joker then
      max_unique_count := max !max_unique_count count
    else
      joker_count := !joker_count + 1
    in
  String.iter hand_iter hand;
  if joker then
    Hashtbl.remove counts 'J';
  max_unique_count := !max_unique_count + !joker_count;
  match Hashtbl.length counts with
  | 5 -> 0 (* high card *)
  | 4 -> 1 (* one pair *)
  | 3 -> if !max_unique_count = 2 then 2 (* two pair *) else 3 (* three of a kind *)
  | 2 -> if !max_unique_count = 3 then 4 (* full house *) else 5 (* four of a kind *)
  | _ -> 6 (* five of a kind *)

let rec compare_ranks joker hand1 hand2 i =
  let rmap = if joker then rank_map_2 else rank_map_1 in
  let rank1 = String.index rmap (String.get hand1 i) in
  let rank2 = String.index rmap (String.get hand2 i) in
  if rank1 = rank2 then
    compare_ranks joker hand1 hand2 (i + 1)
  else
    rank1 - rank2

let compare_hands joker (hand1, _) (hand2, _) =
  let type1 = classify_hand joker hand1 in
  let type2 = classify_hand joker hand2 in
  if type1 = type2 then
    compare_ranks joker hand1 hand2 0
  else
    type1 - type2

let rec parse_input ic =
  try
    let line = input_line ic in
    match String.split_on_char ' ' line with
    | hand :: bid :: [] -> begin
      let bid = int_of_string bid in
      (hand, bid) :: parse_input ic
    end
    | _ -> failwith ""
  with End_of_file ->
    []

let find_score joker hands =
  let sorted_hands = List.sort (compare_hands joker) hands in
  let result = ref 0 in
  let hand_iter idx (_, bid) =
    result := !result + ((idx + 1) * bid) in
  List.iteri hand_iter sorted_hands;
  print_int !result;
  print_endline ""

let () =
  let ic = open_in "input" in
  let hands = parse_input ic in
  close_in ic;
  find_score false hands;
  find_score true hands
