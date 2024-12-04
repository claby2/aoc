open Format

let gather_input (ch : in_channel) : int * int array * int array =
  let num_lines = in_channel_length ch in
  let arr1 = Array.make num_lines 0 in
  let arr2 = Array.make num_lines 0 in
  let rec input (count : int) =
    try
      let s = input_line ch in
      match s with
      | "" -> ()
      | s ->
          let a, b = Scanf.sscanf s "%d %d" (fun a b -> (a, b)) in
          arr1.(count) <- a;
          arr2.(count) <- b;
          input (count + 1)
    with End_of_file -> ()
  in
  input 0;
  (num_lines, arr1, arr2)

let part1 (ch : in_channel) =
  let num_lines, arr1, arr2 = gather_input ch in
  Array.sort compare arr1;
  Array.sort compare arr2;
  let rec loop (total : int) (i : int) =
    if i < num_lines then
      let num = abs (arr1.(i) - arr2.(i)) in
      loop (total + num) (i + 1)
    else total
  in
  printf "%d\n" (loop 0 0)

let part2 (ch : in_channel) =
  let num_lines, arr1, arr2 = gather_input ch in
  let freq = Hashtbl.create num_lines in
  let rec fill_freq (i : int) =
    if i < num_lines then (
      let v = arr2.(i) in
      if Hashtbl.mem freq v then Hashtbl.replace freq v (Hashtbl.find freq v + 1)
      else Hashtbl.add freq v 1;
      fill_freq (i + 1))
  in
  fill_freq 0;
  let rec loop (total : int) (i : int) =
    if i < num_lines then
      let v = arr1.(i) in
      if Hashtbl.mem freq v then
        let count = Hashtbl.find freq v in
        loop (total + (v * count)) (i + 1)
      else loop total (i + 1)
    else total
  in
  printf "%d\n" (loop 0 0)

let () =
  let ch = open_in "01.txt" in
  part1 ch;
  let ch = open_in "01.txt" in
  part2 ch
