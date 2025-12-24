let lines = Aoc.Parser.read_day 10

let parse line =
  let parse_indicator_light str =
    let len = String.length str - 1 in
    let rec aux i acc =
      if i < 0 then acc
      else
        let acc = if str.[i] = '#' then (i - 1) :: acc else acc in
        aux (i - 1) acc
    in
    aux len []
  in
  let parse_comma_sep str =
    let len = String.length str - 2 in
    let str = String.sub str 1 len in
    let ls = String.split_on_char ',' str in
    List.map int_of_string ls
  in
  match String.split_on_char ' ' line with
  | [] -> failwith "Error"
  | h :: r -> (
      let indicator_lights = parse_indicator_light h in
      let r = List.map parse_comma_sep r in
      match List.rev r with
      | [] -> failwith "Error"
      | joltage :: schematics -> (indicator_lights, schematics, joltage))

(* Assume lights are sorted in ascending order *)
let rec merge cur_lights next_lights res =
  match cur_lights with
  | [] -> List.rev_append res next_lights
  | h :: r -> (
      match next_lights with
      | [] -> List.rev_append res cur_lights
      | a :: b ->
          if a = h then merge r b res
          else if a < h then merge cur_lights b (a :: res)
          else merge r next_lights (h :: res))

(* Exhastively search all 2^n solutions (for part 2) *)
let rec solve_p1 schematics cur_lights cur_steps res =
  let res = if cur_lights = [] then cur_steps :: res else res in
  match schematics with
  | [] -> res
  | h :: r ->
      let res = solve_p1 r (merge cur_lights h []) (h :: cur_steps) res in
      solve_p1 r cur_lights cur_steps res

let part1 lines =
  let calc line =
    let indicator_lights, schematics, _ = parse line in
    let res = solve_p1 schematics indicator_lights [] [] in
    List.fold_left (fun acc x -> min acc (List.length x)) 999 res
  in

  List.fold_left (fun acc x -> acc + calc x) 0 lines

module IntListMap = Map.Make (struct
  type t = int list

  let compare = compare
end)

let part2 lines =
  let check joltage = List.fold_left (fun acc x -> acc && x = 0) true joltage in
  let is_valid joltage =
    List.fold_left (fun acc x -> acc && x >= 0) true joltage
  in

  let press schematic joltage =
    let press_idx idx joltage =
      List.mapi (fun i x -> if i = idx then x - 1 else x) joltage
    in
    List.fold_left (fun acc x -> press_idx x acc) joltage schematic
  in

  let rec solve schematics joltage m =
    if check joltage then (0, m)
    else
      match IntListMap.find_opt joltage m with
      | Some x -> (x, m)
      | None ->
          let lights =
            List.filter
              (fun x -> x != -1)
              (List.mapi (fun i x -> if x mod 2 = 1 then i else -1) joltage)
          in
          let solutions = solve_p1 schematics lights [] [] in
          List.fold_left
            (fun (res, m) solution ->
              let new_joltage =
                List.fold_left (fun acc x -> press x acc) joltage solution
              in
              if is_valid new_joltage then
                let new_joltage = List.map (fun x -> x / 2) new_joltage in
                let new_res, m = solve schematics new_joltage m in
                let m = IntListMap.add new_joltage new_res m in
                (min res (List.length solution + (2 * new_res)), m)
              else (res, m))
            (999, m) solutions
  in

  let calc line =
    let _, schematics, joltage = parse line in
    fst (solve schematics joltage IntListMap.empty)
  in

  List.fold_left (fun acc x -> acc + calc x) 0 lines

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 lines) (part2 lines)
