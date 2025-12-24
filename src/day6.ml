let lines = Aoc.Parser.read_day 6

let part1 lines =
  let lines =
    lines
    |> List.map (fun x ->
        String.split_on_char ' ' x |> List.map String.trim
        |> List.filter (fun s -> s <> ""))
  in
  let rec merge_lines x y =
    match x with
    | [] -> []
    | h :: r -> (
        match y with
        | [] -> []
        | a :: b -> (int_of_string h :: a) :: merge_lines r b)
  in
  let acc_lines, ops =
    List.fold_left
      (fun (acc, ops) x ->
        match List.hd x with
        | "*" | "+" -> (acc, x)
        | _ -> (merge_lines x acc, ops))
      (List.init (List.length (List.hd lines)) (fun _ -> []), [])
      lines
  in

  let rec proc acc_lines ops =
    match ops with
    | [] -> 0
    | h :: r -> (
        match acc_lines with
        | [] -> 0
        | a :: b ->
            (match h with
              | "*" -> List.fold_left (fun acc x -> acc * x) 1 a
              | "+" -> List.fold_left (fun acc x -> acc + x) 0 a
              | _ -> 0)
            + proc b r)
  in
  proc acc_lines ops

let part2 lines =
  let lines =
    List.map (fun s -> List.init (String.length s) (String.get s)) lines
  in

  let rec proc lines cur opp res =
    if lines = [] then res + cur
    else
      let lines, curr, op =
        List.fold_left
          (fun (ls, curr, op) x ->
            match x with
            | [] -> (ls, curr, op)
            | h :: r -> (
                match h with
                | '*' -> (r :: ls, curr, '*')
                | '+' -> (r :: ls, curr, '+')
                | ' ' -> (r :: ls, curr, op)
                | a ->
                    ( r :: ls,
                      (int_of_char a - 48 + if curr > 0 then curr * 10 else 0),
                      op )))
          ([], -1, ' ') lines
      in
      let lines = List.rev lines in
      if op = '*' || op = '+' then proc lines curr op res
      else if curr = -1 then proc lines 0 opp (res + cur)
      else proc lines (if opp = '*' then cur * curr else cur + curr) opp res
  in
  proc lines 0 ' ' 0

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 lines) (part2 lines)
