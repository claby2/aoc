open Format

module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

let part1 (ch : in_channel) =
  let extract_numbers (s : string) : IntSet.t =
    IntSet.of_list
      (List.map int_of_string
         (List.filter
            (fun s -> String.length s != 0)
            (List.map String.trim (String.split_on_char ' ' (String.trim s)))))
  in

  let calculate_points (line : string) : int =
    let parts = String.split_on_char ':' line in
    let parts = String.split_on_char '|' (List.nth parts 1) in
    let winning_numbers = extract_numbers (List.nth parts 0) in
    let numbers = extract_numbers (List.nth parts 1) in
    let intersection = IntSet.inter winning_numbers numbers in
    let value =
      int_of_float (2.0 ** float_of_int (IntSet.cardinal intersection - 1))
    in
    value
  in

  let rec loop (total : int) =
    try
      let s = input_line ch in
      loop (total + calculate_points s)
    with End_of_file -> total
  in
  printf "%d\n" (loop 0)

let () =
  let ch = open_in "04.txt" in
  part1 ch
