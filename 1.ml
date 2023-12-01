(* Filename to use for input. *)
let input_filename = "input";;

(* String representation of numbers *)
let number_words = [
  (1, "one");
  (2, "two");
  (3, "three");
  (4, "four");
  (5, "five");
  (6, "six");
  (7, "seven");
  (8, "eight");
  (9, "nine");
];;

let rec find_algorithm direction index line algorithm =
  let c = algorithm index line in
  if c >= 0 then
    c
  else
    find_algorithm direction (index + direction) line algorithm

let find_first algorithm line =
  find_algorithm 1 0 line algorithm

let find_last algorithm line =
  find_algorithm (-1) ((String.length line) - 1) line algorithm

let test_digit index line =
  let c = String.get line index in
  if (c >= '0') && (c <= '9') then
    (int_of_char c) - 0x30
  else
    -1

let rec test_word_presence line line_index word word_index =
  if word_index == (String.length word) then
    true
  else if line_index == (String.length line) then
    false
  else if (String.get line line_index) != (String.get word word_index) then
    false
  else
    test_word_presence line (line_index + 1) word (word_index + 1)

let rec test_number_words index line word_list =
  match word_list with
  | [] -> 0
  | (value, word) :: tail ->
    if test_word_presence line index word 0 then
      value
    else
      test_number_words index line tail

let test_substring_or_digit index line =
  let c = test_digit index line in
  if c >= 0 then
    c
  else
    let word_value = test_number_words index line number_words in
    if word_value > 0 then
      word_value
    else
      -1

let rec calc_day_1 file algorithm =
  try
    let line = input_line file in
    let hi = find_first algorithm line in
    let lo = find_last algorithm line in
    ((hi * 10) + lo) + calc_day_1 file algorithm
  with End_of_file ->
    0

let day_1 algorithm =
  let ic = open_in input_filename in
  try
    print_int (calc_day_1 ic algorithm);
    print_endline "";
    close_in ic
  with e ->
    close_in_noerr ic;
    raise e

let () =
  day_1 test_digit;
  day_1 test_substring_or_digit
