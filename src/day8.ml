let lines = Aoc.Parser.read_day 8

module IntMap = Map.Make (Int)

let lines =
  List.mapi
    (fun i x ->
      match String.split_on_char ',' x with
      | [ a; b; c ] -> (int_of_string a, int_of_string b, int_of_string c, i)
      | _ -> failwith "Error")
    lines

let get_dist (x1, x2, x3) (y1, y2, y3) =
  ((x1 - y1) * (x1 - y1)) + ((x2 - y2) * (x2 - y2)) + ((x3 - y3) * (x3 - y3))

let rec get_edges lines res =
  match lines with
  | [] -> res
  | (a, b, c, i) :: r ->
      let a =
        List.map (fun (x, y, z, j) -> (get_dist (a, b, c) (x, y, z), i, j)) r
      in
      get_edges r (a @ res)

let rec find i roots =
  let r = IntMap.find i roots in
  if r = i then r else find r roots

let idx_ls = List.init (List.length lines) (fun x -> x)

let part1 lines =
  let edges = get_edges lines [] |> List.sort compare in

  let sizes, roots, _ =
    List.fold_left
      (fun (sizes, roots, conn) (_, i, j) ->
        let r1 = find i roots in
        let r2 = find j roots in
        if r1 = r2 || conn >= 1000 then (sizes, roots, conn + 1)
        else
          let s1 = IntMap.find r1 sizes in
          let s2 = IntMap.find r2 sizes in
          if s1 < s2 then
            let sizes = IntMap.add r2 (s1 + s2) sizes in
            let roots = IntMap.add r1 r2 roots in
            (sizes, roots, conn + 1)
          else
            let sizes = IntMap.add r1 (s1 + s2) sizes in
            let roots = IntMap.add r2 r1 roots in
            (sizes, roots, conn + 1))
      ( List.fold_left (fun map k -> IntMap.add k 1 map) IntMap.empty idx_ls,
        List.fold_left (fun map k -> IntMap.add k k map) IntMap.empty idx_ls,
        0 )
      edges
  in
  let roots =
    List.sort_uniq compare
      (List.init (List.length lines) (fun x -> find x roots))
  in
  let sizes =
    List.map (fun x -> IntMap.find x sizes) roots
    |> List.sort compare |> List.rev
  in
  List.hd sizes * List.nth sizes 1 * List.nth sizes 2

let part2 lines =
  let get_res i j =
    let x1, _, _, _ = List.nth lines i in
    let x2, _, _, _ = List.nth lines j in
    x1 * x2
  in

  let edges = get_edges lines [] |> List.sort compare in

  let _, _, res =
    List.fold_left
      (fun (sizes, roots, res) (_, i, j) ->
        let r1 = find i roots in
        let r2 = find j roots in
        if r1 = r2 then (sizes, roots, res)
        else
          let s1 = IntMap.find r1 sizes in
          let s2 = IntMap.find r2 sizes in
          let parent, child = if s1 < s2 then (r2, r1) else (r1, r2) in
          let total = s1 + s2 in
          let sizes = IntMap.add parent total sizes in
          let roots = IntMap.add child parent roots in
          (sizes, roots, if total = List.length lines then get_res i j else res))
      ( List.fold_left (fun map k -> IntMap.add k 1 map) IntMap.empty idx_ls,
        List.fold_left (fun map k -> IntMap.add k k map) IntMap.empty idx_ls,
        0 )
      edges
  in
  res

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 lines) (part2 lines)
