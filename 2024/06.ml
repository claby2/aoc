let explode s = List.init (String.length s) (String.get s)

module Direction = struct
  type t = Up | Right | Down | Left

  let delta = function
    | Up -> (0, -1)
    | Right -> (1, 0)
    | Down -> (0, 1)
    | Left -> (-1, 0)

  let turn_right = function
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up
end

module Map = struct
  type t = { m : bool array; width : int }

  let parse lines =
    match lines with
    | [] -> None
    | first :: _ ->
        let width = String.length first in
        let m =
          lines |> List.map explode |> List.flatten
          |> List.map (function '#' -> true | _ -> false)
          |> Array.of_list
        in
        Some { m; width }

  let get m x y =
    if y < 0 || y >= Array.length m.m / m.width || x < 0 || x >= m.width then
      None
    else try Some m.m.((y * m.width) + x) with _ -> None

  let calculate_distinct_positions ?(initial_direction = Direction.Up) m
      start_pos =
    let visited = Array.make (Array.length m.m) false in
    let sx, sy = start_pos in
    visited.((sy * m.width) + sx) <- true;
    let rec aux m direction x y =
      let dx, dy = Direction.delta direction in
      let x, y = (x + dx, y + dy) in
      match get m x y with
      | None -> ()
      | Some v when v -> aux m (Direction.turn_right direction) (x - dx) (y - dy)
      | Some v ->
          visited.((y * m.width) + x) <- true;
          aux m direction x y
    in
    aux m initial_direction sx sy;
    visited |> Array.to_list |> List.filter (fun x -> x) |> List.length
end

let find_start_pos lines =
  let lines = lines |> List.map explode in
  let rec aux y lines =
    match lines with
    | [] -> None
    | line :: lines -> (
        match List.find_index (fun c -> c = '^') line with
        | None -> aux (y + 1) lines
        | Some x -> Some (x, y))
  in
  aux 0 lines

let () =
  In_channel.with_open_text "06.txt" (fun ic ->
      let lines = In_channel.input_lines ic in
      let start_pos = find_start_pos lines in
      let map = Map.parse lines in
      match (start_pos, map) with
      | Some start_pos, Some map ->
          Printf.printf "%d\n" @@ Map.calculate_distinct_positions map start_pos
      | _ -> ())
