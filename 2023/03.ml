open Format

type symbol = Digit of int | Period | Symbol
type schematic = symbol list list
type coord = { row : int; col : int }

module Board = Map.Make (struct
  type t = coord

  let compare = compare
end)

type 'a board = 'a Board.t

let rec explode (s : string) =
  match s with
  | "" -> []
  | s -> [ String.get s 0 ] @ explode (String.sub s 1 (String.length s - 1))

let rec parse_input (input : string list) : schematic =
  let rec parse_line (s : string) : symbol list =
    match explode s with
    | [] -> []
    | c :: rest ->
        let sym =
          if c >= '0' && c <= '9' then Digit (int_of_char c - int_of_char '0')
          else if c = '.' then Period
          else Symbol
        in
        sym :: parse_line (String.of_seq (List.to_seq rest))
  in
  match input with
  | [] -> []
  | line :: rest -> parse_line line :: parse_input rest

let adjacency_board (s : schematic) : bool board =
  let rec neighbors_of (row : int) (column : int) : coord list =
    printf "Neighbors of %d %d\n" row column;
    [
      { row = row - 1; col = column - 1 };
      { row = row - 1; col = column };
      { row = row - 1; col = column + 1 };
      { row; col = column - 1 };
      { row; col = column + 1 };
      { row = row + 1; col = column - 1 };
      { row = row + 1; col = column };
      { row = row + 1; col = column + 1 };
    ]
  in

  let rec construct (s : schematic) (row : int) (b : bool board) : bool board =
    match s with
    | [] -> b
    | symbols :: rest ->
        let enumerated_symbols = List.mapi (fun i sym -> (i, sym)) symbols in
        let adjacent_coords =
          List.fold_left
            (fun coords (column, sym) ->
              match sym with
              | Symbol -> neighbors_of row column @ coords
              | _ -> coords)
            [] enumerated_symbols
        in
        List.fold_left
          (fun b coord -> Board.add coord true b)
          (construct rest (row + 1) b)
          adjacent_coords
  in
  construct s 0 Board.empty

let rec read_input (ch : in_channel) : string list =
  try
    let s = input_line ch in
    s :: read_input ch
  with End_of_file -> []

let next_number (symbols : symbol list) : int option * symbol list =
  let rec parse_digits (processing : bool) (symbols : symbol list) :
      string * symbol list =
    match symbols with
    | [] -> ("", symbols)
    | Digit d :: rest ->
        let num_string, rest = parse_digits true rest in
        (string_of_int d ^ num_string, rest)
    | _ :: rest -> if processing then ("", symbols) else parse_digits false rest
  in
  let digits = parse_digits false symbols in
  match digits with
  | "", rest -> (None, rest)
  | digits, rest -> (Some (int_of_string digits), rest)

let part1 (ch : in_channel) =
  let rec calculate_sum (s : schematic) (b : bool board) : int =
    let rec part_numbers (symbols : symbol list) (b : bool board) (row : int)
        (column : int) : int list =
      match next_number symbols with
      | None, _ -> []
      | Some num, rest ->
          let num_length = String.length (string_of_int num) in
          printf "ll symbols %d rest %d num %d\n" (List.length symbols)
            (List.length rest) num;
          let start_column =
            column + (List.length symbols - List.length rest - num_length)
          in
          let columns = List.init num_length (fun x -> start_column + x) in
          let num_coords = List.map (fun col -> { row; col }) columns in
          let is_valid =
            List.exists (fun coord -> Board.mem coord b) num_coords
          in
          (if is_valid then (
             printf "Accepted %d start col %d last col %d \n" num start_column
               (List.hd (List.rev columns));
             [ num ])
           else (
             printf "Rejected %d start col %d last col %d \n" num start_column
               (List.hd (List.rev columns));
             []))
          @ part_numbers rest b row
              (column + (List.length symbols - List.length rest))
    in

    let rec calculate (s : schematic) (b : bool board) (row : int) : int =
      match s with
      | [] -> 0
      | symbols :: rest ->
          let sum = List.fold_left ( + ) 0 (part_numbers symbols b row 0) in
          sum + calculate rest b (row + 1)
    in
    calculate s b 0
  in

  let input = read_input ch in
  let schematic = parse_input input in
  let adj_board = adjacency_board schematic in
  printf "%d\n" (calculate_sum schematic adj_board)

let () =
  let ch = open_in "03.txt" in
  part1 ch
