let lines = Aoc.Parser.read_day 11

module StringMap = Map.Make (String)

module StringBoolBoolMap = Map.Make (struct
  type t = string * bool * bool

  let compare = compare
end)

module StringSet = Set.Make (String)

let part1 lines =
  let add line m =
    match String.split_on_char ':' line with
    | [ a; b ] -> StringMap.add a (List.tl (String.split_on_char ' ' b)) m
    | _ -> failwith "error"
  in
  let m = List.fold_left (fun acc x -> add x acc) StringMap.empty lines in
  let rec dfs cur target =
    if cur = target then 1
    else
      List.map
        (fun x -> dfs x target)
        (match StringMap.find_opt cur m with Some x -> x | None -> [])
      |> List.fold_left (fun acc x -> acc + x) 0
  in
  dfs "you" "out"

let part2 lines =
  let add line m =
    match String.split_on_char ':' line with
    | [ a; b ] -> StringMap.add a (List.tl (String.split_on_char ' ' b)) m
    | _ -> failwith "error"
  in
  let m = List.fold_left (fun acc x -> add x acc) StringMap.empty lines in
  let rec dfs cur target fft dac cache =
    if cur = target then if fft && dac then (1, cache) else (0, cache)
    else
      let fft = fft || cur = "fft" in
      let dac = dac || cur = "dac" in
      match StringBoolBoolMap.find_opt (cur, fft, dac) cache with
      | Some x -> (x, cache)
      | None ->
          List.fold_left
            (fun (acc, cache) x ->
              let len, cache = dfs x target fft dac cache in
              let res = acc + len in
              let cache = StringBoolBoolMap.add (x, fft, dac) len cache in
              (res, cache))
            (0, cache)
            (match StringMap.find_opt cur m with Some x -> x | None -> [])
  in
  fst (dfs "svr" "out" false false StringBoolBoolMap.empty)

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 lines) (part2 lines)
