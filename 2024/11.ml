module Line = struct
  type t = int list

  let parse line = line |> String.split_on_char ' ' |> List.map int_of_string

  let rec blink l =
    match l with
    | [] -> []
    | x :: xs ->
        let head =
          if x == 0 then [ 1 ]
          else if String.length (string_of_int x) mod 2 == 0 then
            let half = String.length (string_of_int x) / 2 in
            let first = String.sub (string_of_int x) 0 half in
            let second = String.sub (string_of_int x) half half in
            [ int_of_string first; int_of_string second ]
          else [ x * 2024 ]
        in
        head @ blink xs

  let rec blink_n l = function
    | 0 -> []
    | 1 -> blink l
    | n -> blink_n (blink l) (n - 1)

  let count = List.length
end

let () =
  In_channel.with_open_text "11.txt" (fun ic ->
      let line = In_channel.input_line ic in
      match line with
      | None -> ()
      | Some line ->
          let line = Line.parse line in
          Printf.printf "%d\n" @@ (Line.blink_n line 25 |> Line.count))
