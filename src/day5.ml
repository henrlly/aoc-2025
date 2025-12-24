let lines = Aoc.Parser.read_day 5

let ranges, ids =
  let rec split lines ranges =
    match lines with
    | [] -> (ranges, [])
    | h :: r -> if h = "" then (ranges, r) else split r (h :: ranges)
  in
  let ranges, ids = split lines [] in
  ( List.sort compare
      (List.map
         (fun x ->
           match String.split_on_char '-' x with
           | [ s; e ] -> (int_of_string s, int_of_string e)
           | _ -> failwith "Error")
         ranges),
    List.map (fun x -> int_of_string x) ids )

module IntMap = Map.Make (Int)

let part1 ranges ids =
  let _ranges, (s, e) =
    List.fold_left
      (fun (prev, (ss, ee)) (s, e) ->
        if s <= ee then (prev, (ss, max e ee)) else ((ss, ee) :: prev, (s, e)))
      ([], List.hd ranges)
      (List.tl ranges)
  in
  let ranges = IntMap.of_list ((s, e) :: _ranges) in
  let find_in_range x range =
    let left, data, _ = IntMap.split x range in
    match data with
    | Some e -> e >= x
    | None -> (
        match IntMap.max_binding_opt left with
        (* s is necessarily < x, so just checking if e >= x is enough. *)
        | Some (_, e) -> e >= x
        | None -> false)
  in
  List.fold_left
    (fun acc x -> if find_in_range x ranges then acc + 1 else acc)
    0 ids

let part2 ranges =
  let _ranges, (s, e) =
    List.fold_left
      (fun (prev, (ss, ee)) (s, e) ->
        if s <= ee then (prev, (ss, max e ee)) else ((ss, ee) :: prev, (s, e)))
      ([], List.hd ranges)
      (List.tl ranges)
  in
  let ranges = (s, e) :: _ranges in
  List.fold_left (fun acc (s, e) -> acc + (e - s + 1)) 0 ranges

let () =
  Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 ranges ids) (part2 ranges)
