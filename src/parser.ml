let read_file filename =
  let ch = open_in filename in
  let rec loop acc =
    try loop (input_line ch :: acc)
    with End_of_file ->
      close_in ch;
      List.rev acc
  in
  loop []

let read_day n =
  let path = Printf.sprintf "inputs/day%d.in" n in
  read_file path
