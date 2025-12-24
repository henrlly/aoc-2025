let lines = Aoc.Parser.read_day 7

let part1 lines =
  let append_uniq e ls =
    match ls with [] -> [ e ] | h :: _ -> if h = e then ls else e :: ls
  in

  let rec calc times res ls line idx =
    if idx = String.length line then (times, res)
    else
      match ls with
      | [] -> (times, res)
      | h :: r ->
          if idx = h then
            if String.get line idx = '^' then
              if idx = 0 then
                calc (times + 1) (append_uniq (idx + 1) res) r line (idx + 1)
              else if idx = String.length line - 1 then
                calc (times + 1) (append_uniq (idx - 1) res) r line (idx + 1)
              else
                calc (times + 1)
                  ((idx + 1) :: append_uniq (idx - 1) res)
                  r line (idx + 1)
            else calc times (append_uniq idx res) r line (idx + 1)
          else calc times res ls line (idx + 1)
  in

  match lines with
  | [] -> 0
  | h :: r ->
      let idx = String.index h 'S' in
      let times, _ =
        List.fold_left
          (fun (times, ls) x ->
            let times, ls = calc times [] ls x 0 in
            (times, List.rev ls))
          (0, [ idx ]) r
      in
      times

module IntMap = Map.Make (Int)

let part2 lines =
  let add x opt =
    match opt with None -> Some x | Some old -> Some (old + x)
  in

  let rec calc map new_map line idx =
    if idx = String.length line then new_map
    else
      match IntMap.find_opt idx map with
      | None -> calc map new_map line (idx + 1)
      | Some x ->
          if String.get line idx = '^' && x > 0 then
            let new_map = IntMap.remove idx new_map in
            if idx = 0 then
              let new_map = IntMap.update (idx + 1) (add x) new_map in
              calc map new_map line (idx + 1)
            else if idx = String.length line - 1 then
              let new_map = IntMap.update (idx - 1) (add x) new_map in
              calc map new_map line (idx + 1)
            else
              let new_map = IntMap.update (idx - 1) (add x) new_map in
              let new_map = IntMap.update (idx + 1) (add x) new_map in
              calc map new_map line (idx + 1)
          else calc map new_map line (idx + 1)
  in

  match lines with
  | [] -> 0
  | h :: r ->
      let idx = String.index h 'S' in
      let m =
        List.fold_left
          (fun map x -> calc map map x 0)
          (IntMap.add idx 1 IntMap.empty)
          r
      in
      IntMap.fold (fun _ value acc -> value + acc) m 0

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 lines) (part2 lines)
