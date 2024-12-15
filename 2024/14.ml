module Coord = struct
  type t = { x : int; y : int }

  let make x y = { x; y }
  let compare = compare
end

type quadrant = NW | NE | SW | SE

module Robot = struct
  type t = { p : Coord.t; v : Coord.t }

  let parse line =
    Scanf.sscanf line "p=%d,%d v=%d,%d" (fun a b c d ->
        { p = Coord.make a b; v = Coord.make c d })

  let pos r = r.p

  let move width height r =
    let px =
      (if r.p.x + r.v.x < 0 then width + r.p.x + r.v.x else r.p.x + r.v.x)
      mod width
    in
    let py =
      (if r.p.y + r.v.y < 0 then height + r.p.y + r.v.y else r.p.y + r.v.y)
      mod height
    in
    let res = { p = Coord.make px py; v = r.v } in
    res

  let rec move_n n width height r =
    match n with
    | 0 -> r
    | n -> move_n (n - 1) width height (move width height r)

  let quadrant_opt width height r =
    let mid = Coord.make ((width - 1) / 2) ((height - 1) / 2) in
    match (compare r.p.x mid.x, compare r.p.y mid.y) with
    | x, y when x < 0 && y < 0 -> Some NW
    | x, y when x < 0 && y > 0 -> Some SW
    | x, y when x > 0 && y < 0 -> Some NE
    | x, y when x > 0 && y > 0 -> Some SE
    | _ -> None
end

let evaluate width height robots =
  let extract q =
    List.partition (fun r -> Robot.quadrant_opt width height r = Some q)
  in
  let nw, robots = extract NW robots in
  let ne, robots = extract NE robots in
  let sw, robots = extract SW robots in
  let se, _ = extract SE robots in
  List.length nw * List.length ne * List.length sw * List.length se

module CoordSet = Set.Make (Coord)

let rec find_tree tries width height robots =
  (* Keep searching until all robots are in unique positions *)
  let s = ref CoordSet.empty in
  robots |> List.iter (fun r -> s := CoordSet.add (Robot.pos r) !s);
  if CoordSet.cardinal !s = List.length robots then tries
  else
    find_tree (tries + 1) width height
      (robots |> List.map (Robot.move width height))

let () =
  In_channel.with_open_text "14.txt" (fun ic ->
      let lines = In_channel.input_lines ic in
      let width = 101 in
      let height = 103 in
      let robots = lines |> List.map Robot.parse in
      Printf.printf "%d\n"
      @@ evaluate width height
           (robots |> List.map (Robot.move_n 100 width height));
      Printf.printf "%d\n" @@ find_tree 0 width height robots;
      ())
