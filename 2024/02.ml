module Report = struct
  type t = int list

  let gradual x y =
    let diff = abs (x - y) in
    diff >= 1 && diff <= 3

  let rec satisfies_prop p l =
    match l with
    | [] | [ _ ] -> true
    | x :: y :: xs -> p x y && satisfies_prop p (y :: xs)

  let rec is_increasing (report : t) : bool = report |> satisfies_prop ( < )
  let rec is_decreasing (report : t) : bool = report |> satisfies_prop ( > )
  let rec is_gradual (report : t) : bool = report |> satisfies_prop gradual

  let is_safe (report : t) : bool =
    (is_increasing report || is_decreasing report) && is_gradual report

  let is_tolerable (report : t) =
    let rec aux tolerable p l =
      match l with
      | [] | [ _ ] -> true
      | x :: y :: xs ->
          (p x y && aux tolerable p (y :: xs))
          || (tolerable && aux false p (x :: xs))
    in
    report |> aux true (fun x y -> x < y && gradual x y)
    || report |> aux true (fun x y -> x > y && gradual x y)
    || List.tl report |> aux false (fun x y -> x < y && gradual x y)
    || List.tl report |> aux false (fun x y -> x > y && gradual x y)
end

let parse_reports (lines : string list) : Report.t list =
  lines
  |> List.map (fun line ->
         line |> String.split_on_char ' ' |> List.map int_of_string)

let () =
  In_channel.with_open_text "02.txt" (fun ic ->
      let reports = In_channel.input_lines ic |> parse_reports in
      Printf.printf "%d\n"
      @@ (reports |> List.filter Report.is_safe |> List.length);
      Printf.printf "%d\n"
      @@ (reports |> List.filter Report.is_tolerable |> List.length))
