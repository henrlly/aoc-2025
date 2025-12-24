let lines = Aoc.Parser.read_day 2

let nums =
  List.hd lines |> String.split_on_char ','
  |> List.map (fun x -> Scanf.sscanf x "%d-%d" (fun a b -> (a, b)))

let rec part1 nums =
  let is_invalid num =
    let s = string_of_int num in
    let len = String.length s in
    if len mod 2 <> 0 then false
    else String.sub s 0 (len / 2) = String.sub s (len / 2) (len / 2)
  in

  match nums with
  | [] -> 0
  | s :: r ->
      let start_i, end_i = s in
      List.fold_left
        (fun acc x -> acc + if is_invalid x then x else 0)
        0
        (List.init (end_i - start_i + 1) (( + ) start_i))
      + part1 r

let rec part2 nums =
  let primes = [ 2; 3; 5; 7; 9; 13; 17 ] in

  let rec is_repeated s sub_len start_i =
    if start_i = 0 then true
    else if String.sub s 0 sub_len = String.sub s start_i sub_len then
      is_repeated s sub_len (start_i - sub_len)
    else false
  in

  let rec is_invalid num p =
    match p with
    | [] -> false
    | h :: r ->
        let s = string_of_int num in
        let len = String.length s in
        if len mod h <> 0 then is_invalid num r
        else if is_repeated s (len / h) (len - (len / h)) then true
        else is_invalid num r
  in

  match nums with
  | [] -> 0
  | s :: r ->
      let start_i, end_i = s in
      List.fold_left
        (fun acc x -> acc + if is_invalid x primes then x else 0)
        0
        (List.init (end_i - start_i + 1) (( + ) start_i))
      + part2 r

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 nums) (part2 nums)
