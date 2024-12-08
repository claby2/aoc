let explode s = List.init (String.length s) (String.get s)

module Coord = struct
  type t = int * int

  let make x y = (x, y)

  let compare (x1, y1) (x2, y2) =
    let c = compare x1 x2 in
    if c <> 0 then c else compare y1 y2

  let delta (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
  let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  let negate (x, y) = (-x, -y)
  let outside width height (x, y) = x < 0 || x >= width || y < 0 || y >= height

  let rec add_until c delta prop =
    if prop c then [] else c :: add_until (add c delta) delta prop
end

module CoordSet = Set.Make (Coord)

module Map = struct
  type t = { locations : (char, Coord.t list) Hashtbl.t }

  let parse lines =
    let locations = Hashtbl.create 0 in
    let rec aux y = function
      | [] -> ()
      | line :: lines ->
          let line = explode line in
          line
          |> List.mapi (fun i c -> (c, i))
          |> List.filter (fun (c, i) -> c <> '.')
          |> List.iter (fun (c, x) ->
                 match Hashtbl.find_opt locations c with
                 | None -> Hashtbl.add locations c [ Coord.make x y ]
                 | Some rest ->
                     Hashtbl.replace locations c (Coord.make x y :: rest));
          aux (y + 1) lines
    in
    aux 0 lines;
    { locations }

  let count_antinodes m width height =
    let antinodes = ref [] in
    m.locations
    |> Hashtbl.iter (fun _ coords ->
           let new_antinodes =
             coords
             |> List.map (fun coord1 ->
                    coords
                    |> List.filter (fun coord2 -> coord1 <> coord2)
                    |> List.map (fun coord2 -> Coord.delta coord2 coord1)
                    |> List.map Coord.negate
                    |> List.map (Coord.add coord1))
             |> List.flatten
           in
           antinodes := !antinodes @ new_antinodes);
    let antinodes =
      CoordSet.of_list !antinodes
      |> CoordSet.filter (fun coord -> not (Coord.outside width height coord))
    in
    CoordSet.cardinal antinodes

  let count_antinodes_extended m width height =
    let antinodes = ref [] in
    m.locations
    |> Hashtbl.iter (fun c coords ->
           let new_antinodes =
             coords
             |> List.map (fun coord1 ->
                    coords
                    |> List.filter (fun coord2 -> coord1 <> coord2)
                    |> List.map (fun coord2 ->
                           Coord.add_until coord1
                             (Coord.negate (Coord.delta coord2 coord1))
                             (Coord.outside width height))
                    |> List.flatten)
             |> List.flatten
           in
           antinodes := !antinodes @ new_antinodes);
    let antinodes = CoordSet.of_list !antinodes in
    CoordSet.cardinal antinodes
end

let () =
  In_channel.with_open_text "08.txt" (fun ic ->
      let lines = In_channel.input_lines ic in
      match lines with
      | first :: _ ->
          let width = String.length first in
          let height = List.length lines in
          let map = Map.parse lines in
          let num_antinodes = Map.count_antinodes map width height in
          Printf.printf "%d\n" num_antinodes;
          let num_antinodes = Map.count_antinodes_extended map width height in
          Printf.printf "%d\n" num_antinodes
      | _ -> raise (Invalid_argument "invalid input"))
