type block = Empty | File of int

module Segment = struct
  type t = { size : int; block : block }

  let make size block = { size; block }
  let size s = s.size
  let block s = s.block
end

module Disk = struct
  type t = Segment.t array

  let parse line =
    let id = ref 0 in
    let d = ref [] in
    let is_file = ref true in
    line
    |> String.iter (fun c ->
           let count = int_of_char c - int_of_char '0' in
           if !is_file then (
             d := !d @ [ Segment.make count (File !id) ];
             id := !id + 1)
           else d := !d @ [ Segment.make count Empty ];
           is_file := not !is_file);
    Array.of_list !d

  let print d =
    Array.to_list d
    |> List.iter (fun s ->
           match Segment.block s with
           | Empty -> Printf.printf "%s" @@ String.make (Segment.size s) '.'
           | File id ->
               Printf.printf "%s"
               @@ String.make (Segment.size s) (Char.chr (id + int_of_char '0')));
    Printf.printf "\n"

  let simple_checksum d =
    let rec aux index l loffset r roffset =
      if l >= r && not (l == r && loffset == 0) then 0
      else
        let left = d.(l) in
        let right = d.(r) in
        if loffset >= Segment.size left then aux index (l + 1) 0 r roffset
        else if roffset >= Segment.size right then aux index l loffset (r - 1) 0
        else
          match (Segment.block left, Segment.block right) with
          | Empty, Empty -> aux index l loffset (r - 1) 0
          | Empty, File id ->
              (id * index) + aux (index + 1) l (loffset + 1) r (roffset + 1)
          | File id, _ ->
              (id * index) + aux (index + 1) l (loffset + 1) r roffset
    in
    aux 0 0 0 (Array.length d - 1) 0
end

let () =
  In_channel.with_open_text "09.txt" (fun ic ->
      let lines = In_channel.input_lines ic in
      match lines with
      | first :: _ ->
          let disk = Disk.parse first in
          Printf.printf "%d\n" @@ Disk.simple_checksum disk
      | _ -> raise (Invalid_argument "invalid input"))
