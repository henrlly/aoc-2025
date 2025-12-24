let lines = Aoc.Parser.read_day 1

let rec part1 xs num =
  match xs with
  | [] -> 0
  | s :: r ->
      let dir = s.[0] = 'L' in
      let n = int_of_string (String.sub s 1 (String.length s - 1)) in
      let new_num = (if dir then num - n else num + n) mod 100 in
      (if new_num = 0 then 1 else 0) + part1 r new_num

(* count number of times passes though or reaches 0 *)
let rec part2 xs num =
  match xs with
  | [] -> 0
  | s :: r ->
      let dir = s.[0] = 'L' in
      let n = int_of_string (String.sub s 1 (String.length s - 1)) in
      let new_num = if dir then num - n else num + n in
      (if num > 0 && new_num <= 0 then 1 else 0)
      + (abs new_num / 100)
      + part2 r (((new_num mod 100) + 100) mod 100)

let () =
  Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 lines 50) (part2 lines 50)
