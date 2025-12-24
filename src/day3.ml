let lines = Aoc.Parser.read_day 3

let rec part1 lines =
  let rec b2 s m p1 =
    match s with
    | [] -> if m < 0 then 0 else (p1 * 10) + m
    | h :: r -> b2 r (max m (int_of_char h - 48)) p1
  in
  let rec b1 s =
    match s with
    | [] -> 0
    | h :: r -> max (b1 r) (b2 r (-1) (int_of_char h - 48))
  in

  match lines with
  | [] -> 0
  | s :: r -> b1 (s |> String.to_seq |> List.of_seq) + part1 r

let part2 lines =
  let rec get_val x rest =
    match x with [] -> rest | h :: r -> get_val r ((rest * 10) + h)
  in
  let rec remove_min x =
    match x with
    | [] -> []
    | h :: r ->
        if r = [] then []
        else
          let rest = remove_min r in
          if get_val r 0 > get_val (h :: rest) 0 then r else h :: rest
  in
  let rec b1 s cur m =
    match s with
    | [] -> get_val cur 0
    | h :: r ->
        if List.length cur < 12 then b1 r (h :: cur) h
        else if h >= m then b1 r (h :: remove_min cur) (max m h)
        else b1 r cur m
  in

  List.fold_left
    (fun acc s ->
      acc
      + b1
          (s |> String.to_seq |> List.of_seq |> List.rev
          |> List.map (fun x -> int_of_char x - 48))
          [] 0)
    0 lines

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 lines) (part2 lines)
