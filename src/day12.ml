let lines = Aoc.Parser.read_day 12

let part1 lines =
  let rec get_num_squares lines res =
    match lines with
    | _ :: b :: c :: d :: _ :: t ->
        if String.contains b '.' || String.contains b '#' then
          let count str =
            String.fold_left
              (fun count char -> if char = '#' then count + 1 else count)
              0 str
          in
          let total = count b + count c + count d in
          get_num_squares t (total :: res)
        else get_num_squares t res
    | _ -> res
  in
  let num_squares = List.rev (get_num_squares lines []) in

  let lines = List.filter (fun x -> String.contains x 'x') lines in

  List.fold_left
    (fun acc x ->
      match String.split_on_char ':' x with
      | [ a; b ] -> (
          match String.split_on_char 'x' a with
          | [ x; y ] ->
              let nums = List.tl (String.split_on_char ' ' b) in
              let total_squares = int_of_string x / 3 * (int_of_string y / 3) in
              let num_shapes =
                List.fold_left (fun acc x -> int_of_string x + acc) 0 nums
              in
              let total_tiles = int_of_string x * int_of_string y in
              let num_tiles =
                List.mapi
                  (fun i x -> List.nth num_squares i * int_of_string x)
                  nums
                |> List.fold_left (fun acc x -> acc + x) 0
              in
              acc
              +
              if num_shapes <= total_squares then 1
              else if num_tiles > total_tiles then 0
              else failwith "Cannot determine if can be packed"
          | _ -> failwith "Error")
      | _ -> failwith "Error")
    0 lines

let () = Printf.printf "Part 1: %d\nNo Part 2!\n" (part1 lines)
