module Machine = struct
  type t = { ax : int; ay : int; bx : int; by : int; px : int; py : int }

  let make ax ay bx by px py = { ax; ay; bx; by; px; py }

  let solve_opt m =
    (* axa + bxb = px *)
    (* aya + byb = py *)
    let det = (m.ax * m.by) - (m.bx * m.ay) in
    if det == 0 then None
    else
      let a = (m.px * m.by) - (m.bx * m.py) in
      let b = (m.ax * m.py) - (m.px * m.ay) in
      if a mod det != 0 || b mod det != 0 then None else Some (a / det, b / det)

  let tokens_required m =
    match solve_opt m with None -> None | Some (a, b) -> Some ((3 * a) + b)

  let tokens_required_fixed m =
    let m =
      {
        ax = m.ax;
        ay = m.ay;
        bx = m.bx;
        by = m.by;
        px = m.px + 10000000000000;
        py = m.py + 10000000000000;
      }
    in
    match solve_opt m with None -> None | Some (a, b) -> Some ((3 * a) + b)
end

let parse_button line =
  Scanf.sscanf line "Button %s X+%d, Y+%d" (fun _ x y -> (x, y))

let parse_prize line = Scanf.sscanf line "Prize: X=%d, Y=%d" (fun x y -> (x, y))

let parse lines =
  let rec aux = function
    | l1 :: l2 :: l3 :: _ :: lines | l1 :: l2 :: l3 :: lines ->
        let ax, ay = parse_button l1 in
        let bx, by = parse_button l2 in
        let px, py = parse_prize l3 in
        Machine.make ax ay bx by px py :: aux lines
    | _ -> []
  in
  aux lines

let rec solve f machines =
  machines
  |> List.map (fun m -> Option.value (f m) ~default:0)
  |> List.fold_left ( + ) 0

let () =
  In_channel.with_open_text "13.txt" (fun ic ->
      let machines = parse (In_channel.input_lines ic) in
      let part1 = machines |> solve Machine.tokens_required in
      Printf.printf "%d\n" @@ part1;
      let part2 = machines |> solve Machine.tokens_required_fixed in
      Printf.printf "%d\n" @@ part2)
