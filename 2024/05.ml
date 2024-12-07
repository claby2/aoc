module Rules = struct
  type t = { adj : bool array array; translate : (int, int) Hashtbl.t }
  type rule = int * int

  let parse lines =
    let rules : rule list =
      lines |> List.map (fun l -> Scanf.sscanf l "%d|%d" (fun a b -> (a, b)))
    in
    let current = ref 0 in
    let translate = Hashtbl.create (List.length rules) in
    rules
    |> List.iter (fun (a, b) ->
           if not (Hashtbl.mem translate a) then (
             Hashtbl.add translate a !current;
             current := !current + 1);
           if not (Hashtbl.mem translate b) then (
             Hashtbl.add translate b !current;
             current := !current + 1));
    let adj = Array.make_matrix !current !current false in
    rules
    |> List.iter (fun (a, b) ->
           let a = Hashtbl.find_opt translate a |> Option.get in
           let b = Hashtbl.find_opt translate b |> Option.get in
           adj.(a).(b) <- true);
    { adj; translate }

  let has_rule t a b =
    let a = Hashtbl.find_opt t.translate a |> Option.get in
    let b = Hashtbl.find_opt t.translate b |> Option.get in
    t.adj.(a).(b)
end

let is_valid rules update =
  let update_rev = update |> List.rev in
  let rec aux l =
    match l with
    | [] -> true
    | x :: xs ->
        let invalid = xs |> List.exists (Rules.has_rule rules x) in
        if invalid then false else aux xs
  in
  aux update_rev

let parse_split lines : string list * string list =
  let rec parse_rules lines =
    match lines with
    | [] -> ([], [])
    | x :: xs when x = "" -> ([], xs)
    | x :: xs ->
        let rules, rest = parse_rules xs in
        (x :: rules, rest)
  in
  let rules, rest = parse_rules lines in
  (rules, rest)

let get_middle l =
  let len = List.length l in
  let mid = len / 2 in
  List.nth l mid

let reordered_middle rules update =
  let update =
    update
    |> List.map (fun x ->
           (update |> List.filter (Rules.has_rule rules x) |> List.length, x))
    |> List.sort compare |> List.map snd
  in
  get_middle update

let () =
  In_channel.with_open_text "05.txt" (fun ic ->
      let lines = In_channel.input_lines ic in
      let rules, updates = parse_split lines in
      let rules = Rules.parse rules in
      let updates =
        updates
        |> List.map (fun u ->
               u |> String.split_on_char ',' |> List.map int_of_string)
      in
      let valid_updates, invalid_updates =
        updates |> List.partition (is_valid rules)
      in
      let res =
        valid_updates |> List.map get_middle |> List.fold_left ( + ) 0
      in
      Printf.printf "%d\n" @@ res;
      let res =
        invalid_updates
        |> List.map (reordered_middle rules)
        |> List.fold_left ( + ) 0
      in
      Printf.printf "%d\n" @@ res)
