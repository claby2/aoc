module CoordSet = Set.Make (struct
  type t = int * int

  let compare = compare
end)

module Garden = struct
  type t = { g : char array; width : int }

  let parse lines =
    match lines with
    | [] -> None
    | first :: _ ->
        let width = String.length first in
        let g = ref [] in
        lines |> List.iter (fun l -> l |> String.iter (fun c -> g := c :: !g));
        Some { g = Array.of_list (!g |> List.rev); width }

  let print g =
    for y = 0 to (Array.length g.g / g.width) - 1 do
      for x = 0 to g.width - 1 do
        Printf.printf "%c" g.g.((y * g.width) + x)
      done;
      Printf.printf "\n"
    done

  let get g x y =
    if x < 0 || y < 0 || x >= g.width || y >= Array.length g.g / g.width then
      None
    else Some g.g.((y * g.width) + x)

  let get_neighbors g x y =
    let directions = [ (0, -1); (1, 0); (0, 1); (-1, 0) ] in
    List.map (fun (dx, dy) -> get g (x + dx) (y + dy)) directions

  let perimeters g =
    let height = Array.length g.g / g.width in
    let rec aux x y =
      if x >= g.width then aux 0 (y + 1)
      else if y >= height then []
      else
        match get g x y with
        | None -> []
        | Some c ->
            let neighbors = get_neighbors g x y in
            let p =
              neighbors
              |> List.filter (fun v ->
                     match v with
                     | None -> true
                     | Some c' when c' != c -> true
                     | _ -> false)
              |> List.length
            in
            p :: aux (x + 1) y
    in
    Array.of_list (aux 0 0)

  let calculate_price g =
    let p = perimeters g in
    let visited = ref CoordSet.empty in
    let rec aux c x y =
      if CoordSet.mem (x, y) !visited then (0, 0)
      else
        match get g x y with
        | Some c' when c == c' ->
            visited := CoordSet.add (x, y) !visited;
            let directions = [ (0, -1); (0, 1); (1, 0); (-1, 0) ] in
            directions
            |> List.map (fun (dx, dy) -> aux c (x + dx) (y + dy))
            |> List.fold_left
                 (fun acc (a, p) ->
                   let a', p' = acc in
                   (a' + a, p' + p))
                 (1, p.((y * g.width) + x))
        | _ -> (0, 0)
    in
    let price = ref 0 in
    for y = 0 to (Array.length g.g / g.width) - 1 do
      for x = 0 to g.width - 1 do
        let a, p = aux g.g.((y * g.width) + x) x y in
        price := !price + (a * p)
      done
    done;
    !price
end

let () =
  In_channel.with_open_text "12.txt" (fun ic ->
      let lines = In_channel.input_lines ic in
      let g = Garden.parse lines in
      match g with
      | None -> ()
      | Some g -> Printf.printf "%d\n" @@ Garden.calculate_price g)