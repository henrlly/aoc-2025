let lines = Aoc.Parser.read_day 4

module Coord = struct
  type t = int * int

  let compare = compare
end

module Grid = Set.Make (Coord)

let part1 lines =
  let rolls =
    lines
    |> List.mapi (fun r line ->
        List.init (String.length line) (fun c -> (r, c, line.[c])))
    |> List.flatten
    |> List.fold_left
         (fun set (r, c, char) ->
           if char = '@' then Grid.add (r, c) set else set)
         Grid.empty
  in
  let delta =
    [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]
  in
  let is_valid (r, c) =
    List.fold_left
      (fun res (dr, dc) ->
        if Grid.mem (r + dr, c + dc) rolls then res + 1 else res)
      0 delta
    < 4
  in

  let accessible_rolls = Grid.filter (fun coord -> is_valid coord) rolls in
  Grid.cardinal accessible_rolls

let part2 lines =
  let rolls =
    lines
    |> List.mapi (fun r line ->
        List.init (String.length line) (fun c -> (r, c, line.[c])))
    |> List.flatten
    |> List.fold_left
         (fun set (r, c, char) ->
           if char = '@' then Grid.add (r, c) set else set)
         Grid.empty
  in
  let delta =
    [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]
  in

  let is_valid (r, c) rolls =
    Grid.mem (r, c) rolls
    && List.fold_left
         (fun res (dr, dc) ->
           if Grid.mem (r + dr, c + dc) rolls then res + 1 else res)
         0 delta
       < 4
  in

  let worklist =
    Grid.elements (Grid.filter (fun coord -> is_valid coord rolls) rolls)
  in

  let get_neighbors (r, c) rolls =
    List.fold_left
      (fun res (dr, dc) ->
        if is_valid (r + dr, c + dc) rolls then (r + dr, c + dc) :: res else res)
      [] delta
  in

  let rec proc worklist rolls res =
    match worklist with
    | [] -> res
    | h :: r ->
        if is_valid h rolls then
          let rolls = Grid.remove h rolls in
          let neighbors = get_neighbors h rolls in
          proc (neighbors @ r) rolls (res + 1)
        else proc r rolls res
  in
  proc worklist rolls 0

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 lines) (part2 lines)
