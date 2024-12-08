module Equation = struct
  type t = { goal : int; values : int list }

  let goal e = e.goal

  let parse line =
    match String.split_on_char ':' line with
    | [ goal; values ] ->
        let goal = int_of_string goal in
        let values =
          values |> String.split_on_char ' ' |> List.tl
          |> List.map int_of_string
        in
        Some { goal; values }
    | _ -> None

  let is_valid operators e =
    let rec aux values =
      match values with
      | [] -> []
      | [ x ] -> [ x ]
      | x :: y :: xs ->
          operators |> List.map (fun op -> aux (op x y :: xs)) |> List.flatten
    in
    aux e.values |> List.exists (fun v -> v = e.goal)
end

let rec parse = function
  | [] -> Some []
  | line :: lines -> (
      match Equation.parse line with
      | Some equation -> (
          match parse lines with
          | Some equations -> Some (equation :: equations)
          | None -> None)
      | None -> None)

let concatenate x y =
  let x, y = (string_of_int x, string_of_int y) in
  int_of_string (x ^ y)

let () =
  In_channel.with_open_text "07.txt" (fun ic ->
      let lines = In_channel.input_lines ic in
      let equations = parse lines in
      match equations with
      | Some equations ->
          let part1 =
            equations
            |> List.filter (Equation.is_valid [ ( + ); ( * ) ])
            |> List.map (fun e -> Equation.goal e)
            |> List.fold_left ( + ) 0
          in
          Printf.printf "%d\n" part1;
          let part2 =
            equations
            |> List.filter (Equation.is_valid [ ( + ); ( * ); concatenate ])
            |> List.map (fun e -> Equation.goal e)
            |> List.fold_left ( + ) 0
          in
          Printf.printf "%d\n" part2
      | None -> raise (Invalid_argument "Invalid input"))
