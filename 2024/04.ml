let explode s = List.init (String.length s) (String.get s)

module Board = struct
  type t = { b : char array; width : int }

  let make b w = { b; width = w }

  let get board x y =
    if
      x < 0 || y < 0 || x >= board.width
      || y >= Array.length board.b / board.width
    then None
    else try Some board.b.((y * board.width) + x) with _ -> None

  (* Count the number of times s appears in the board at position (x, y) *)
  let count_occurrences_at board x y s : int =
    let s = explode s in
    let rec aux x y s (dx, dy) =
      match s with
      | [] -> true
      | c :: cs -> (
          match get board x y with
          | Some c' when c' = c -> aux (x + dx) (y + dy) cs (dx, dy)
          | _ -> false)
    in
    let directions = ref [] in
    for dx = -1 to 1 do
      for dy = -1 to 1 do
        if dx <> 0 || dy <> 0 then directions := (dx, dy) :: !directions
      done
    done;
    !directions
    |> List.map (aux x y s)
    |> List.filter (fun x -> x)
    |> List.length

  (* Count the total occurrences of s in the board *)
  let count_occurrences board s =
    let rec aux x y =
      if x >= board.width then aux 0 (y + 1)
      else if y >= Array.length board.b / board.width then 0
      else count_occurrences_at board x y s + aux (x + 1) y
    in
    aux 0 0

  let count_x_mas board =
    let check x y =
      let vicinity = ref [] in
      for i = 0 to 2 do
        for j = 0 to 2 do
          vicinity := get board (x + j) (y + i) :: !vicinity
        done
      done;
      let vicinity = List.filter_map (fun v -> v) !vicinity in
      match vicinity with
      | [ 'M'; _; 'M'; _; 'A'; _; 'S'; _; 'S' ]
      | [ 'M'; _; 'S'; _; 'A'; _; 'M'; _; 'S' ]
      | [ 'S'; _; 'M'; _; 'A'; _; 'S'; _; 'M' ]
      | [ 'S'; _; 'S'; _; 'A'; _; 'M'; _; 'M' ] ->
          true
      | _ -> false
    in
    let rec aux x y =
      if x >= board.width then aux 0 (y + 1)
      else if y >= Array.length board.b / board.width then 0
      else (if check x y then 1 else 0) + aux (x + 1) y
    in
    aux 0 0
end

let () =
  In_channel.with_open_text "04.txt" (fun ic ->
      let lines = In_channel.input_lines ic in
      match lines with
      | [] -> ()
      | first :: _ ->
          let width = String.length first in
          let board =
            lines |> List.map explode |> List.flatten |> Array.of_list
          in
          let board = Board.make board width in
          Printf.printf "%d\n" @@ Board.count_occurrences board "XMAS";
          Printf.printf "%d\n" @@ Board.count_x_mas board)
