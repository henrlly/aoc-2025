let lines = Aoc.Parser.read_day 9

let lines =
  List.map
    (fun x ->
      match String.split_on_char ',' x with
      | [ a; b ] -> (int_of_string a, int_of_string b)
      | _ -> failwith "Error")
    lines

let part1 lines =
  let area (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1) in
  let rec solve lines res =
    match lines with
    | [] -> res
    | h :: r -> solve r (List.fold_left (fun acc x -> max acc (area h x)) res r)
  in
  solve lines 0

let part2 lines =
  let area (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1) in
  match lines with
  | [] -> 0
  | h :: r ->
      let edges, last =
        List.fold_left (fun (acc, prev) x -> ((x, prev) :: acc, x)) ([], h) r
      in
      let edges = (last, h) :: edges in
      (* check if edge (a1, b1) (a2, b2) intesect the rectangle (x1, y1) (x2, y2) *)
      let check_intersect (x1, y1) (x2, y2) (a1, b1) (a2, b2) =
        (* vertical edge *)
        if a1 = a2 then
          (* above or below rect *)
          if min b1 b2 >= max y1 y2 || max b1 b2 <= min y1 y2 then false
          else (a1 < x1 && a1 > x2) || (a1 > x1 && a1 < x2)
        else if
          (* left or right of rect *)
          min a1 a2 >= max x1 x2 || max a1 a2 <= min x1 x2
        then false
        else (b1 < y1 && b1 > y2) || (b1 > y1 && b1 < y2)
      in
      let check_valid (x1, y1) (x2, y2) =
        let intersect =
          List.fold_left
            (fun acc (a, b) -> acc || check_intersect (x1, y1) (x2, y2) a b)
            false edges
        in
        not intersect
      in

      let rec solve lines res =
        match lines with
        | [] -> res
        | h :: r ->
            solve r
              (List.fold_left
                 (fun acc x ->
                   if check_valid h x then max acc (area h x) else acc)
                 res r)
      in
      solve lines 0

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 lines) (part2 lines)
