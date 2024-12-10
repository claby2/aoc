let explode s = List.init (String.length s) (String.get s)

module CoordSet = Set.Make (struct
  type t = int * int

  let compare = compare
end)

module CoordListSet = Set.Make (struct
  type t = (int * int) list

  let compare = compare
end)

module Map = struct
  type t = { m : int array; width : int }

  let parse lines =
    match lines with
    | [] -> None
    | first :: _ ->
        let width = String.length first in
        let m =
          lines
          |> List.map (fun s ->
                 explode s
                 |> List.map (fun c -> int_of_char c - int_of_char '0'))
          |> List.flatten
        in
        Some { m = Array.of_list m; width }

  let get m x y =
    if x < 0 || x >= m.width || y < 0 || y >= Array.length m.m / m.width then
      None
    else Some m.m.((y * m.width) + x)

  let explore m x y =
    let rec aux visited last x y =
      let current = get m x y in
      match current with
      | None -> []
      | Some current ->
          if not (CoordSet.mem (x, y) visited) then
            let visited = CoordSet.add (x, y) visited in
            if current - last != 1 then []
            else if current == 9 then [ (x, y) ]
            else
              let directions = [ (0, 1); (0, -1); (1, 0); (-1, 0) ] in
              directions
              |> List.map (fun (dx, dy) ->
                     aux visited current (x + dx) (y + dy))
              |> List.flatten
          else []
    in
    let visited = CoordSet.empty in
    CoordSet.cardinal (CoordSet.of_list (aux visited (-1) x y))

  let score f m =
    let rec aux x y =
      if x >= m.width then aux 0 (y + 1)
      else if y >= Array.length m.m / m.width then 0
      else f m x y + aux (x + 1) y
    in
    aux 0 0

  let explore2 m x y =
    let rec aux visited last x y =
      let current = get m x y in
      match current with
      | None -> []
      | Some current ->
          if not (CoordSet.mem (x, y) visited) then
            let visited = CoordSet.add (x, y) visited in
            if current - last != 1 then []
            else if current == 9 then [ [ (x, y) ] ]
            else
              let directions = [ (0, 1); (0, -1); (1, 0); (-1, 0) ] in
              directions
              |> List.map (fun (dx, dy) ->
                     aux visited current (x + dx) (y + dy))
              |> List.flatten
              |> List.map (fun l -> (x, y) :: l)
          else []
    in
    let visited = CoordSet.empty in
    CoordListSet.cardinal (CoordListSet.of_list (aux visited (-1) x y))
end

let () =
  In_channel.with_open_text "10.txt" (fun ic ->
      let lines = In_channel.input_lines ic in
      let map = Map.parse lines in
      match map with
      | Some map ->
          let res = Map.score Map.explore map in
          Printf.printf "%d\n" res;
          let res = Map.score Map.explore2 map in
          Printf.printf "%d\n" res
      | None -> ())
