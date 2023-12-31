open Core

type coordinate = { x : int; y : int }

let new_coordinate x y = { x; y }

type number = { num : int; start : coordinate; stop : coordinate }

let new_number num start stop = { num; start; stop }
let explode_string s = List.init (String.length s) ~f:(String.get s)
let x_max = 140
let y_max = 140

let get_matrix x_max y_max lines =
  let matrix = Base.Array.make_matrix ~dimx:x_max ~dimy:y_max '.' in
  let rec add_line_to_matrix matrix x = function
    | [] -> ()
    | line :: rest ->
        let chars = explode_string line in
        let rec add_char_to_matrix matrix y = function
          | [] -> ()
          | c :: rest ->
              matrix.(x).(y) <- c;
              add_char_to_matrix matrix (y + 1) rest
        in
        add_char_to_matrix matrix 0 chars;
        add_line_to_matrix matrix (x + 1) rest
  in
  add_line_to_matrix matrix 0 lines;
  matrix

let print_matrix matrix =
  Base.Array.iter matrix ~f:(fun row ->
      Base.Array.iter row ~f:(fun ele -> Out_channel.output_char stdout ele);
      print_string "\n")

type accumulator = { numbers : number list; stack : string option; start : coordinate option }

let new_accumulator numbers stack start = { numbers; stack; start }

let close_stack (accumulator : accumulator) stop =
  match accumulator.stack with
  | None -> accumulator
  | Some number_string ->
      let num =
        new_number (int_of_string number_string) (Option.value_exn accumulator.start) stop
      in
      { numbers = num :: accumulator.numbers; stack = None; start = None }

let add_to_stack c coord accumulator =
  match accumulator.stack with
  | None -> { accumulator with stack = Some (Char.to_string c); start = Some coord }
  | Some s -> { accumulator with stack = Some (s ^ Char.to_string c) }

let is_digit = function '0' .. '9' -> true | _ -> false

let parse_numbers y_max matrix =
  Base.Array.foldi matrix ~init:[] ~f:(fun x acc row ->
      let accumulator =
        Base.Array.foldi row ~init:(new_accumulator acc None None) ~f:(fun y accumulator c ->
            if is_digit c then add_to_stack c (new_coordinate x y) accumulator
            else close_stack accumulator (new_coordinate x (y - 1)))
      in
      let accumulator = close_stack accumulator (new_coordinate x (y_max - 1)) in
      accumulator.numbers)

let generate_check_coordinates x_max y_max (num : number) =
  let x = num.start.x in
  let y_low = if num.start.y > 0 then num.start.y - 1 else num.start.y in
  let y_high = if num.stop.y < y_max then num.stop.y + 1 else num.stop.y in
  let check_coordinates = [ new_coordinate x y_low; new_coordinate x y_high ] in
  let rec generate_row_check_coordinates check_coordinates row y_max current_y =
    if current_y > y_max then check_coordinates
    else
      generate_row_check_coordinates
        (new_coordinate row current_y :: check_coordinates)
        row y_max (current_y + 1)
  in
  let check_coordinates =
    if x > 0 then generate_row_check_coordinates check_coordinates (x - 1) y_high y_low
    else check_coordinates
  in
  if x < x_max then generate_row_check_coordinates check_coordinates (x + 1) y_high y_low
  else check_coordinates

let is_symbol = function '.' -> false | c -> not (is_digit c)

let check_number x_max y_max (matrix : char array array) (num : number) =
  let check_coordinates = generate_check_coordinates x_max y_max num in
  let rec _check_number matrix = function
    | [] -> false
    | coord :: coords ->
        if is_symbol matrix.(coord.x).(coord.y) then true else _check_number matrix coords
  in
  _check_number matrix check_coordinates

let solve f =
  let lines = In_channel.read_lines f in
  let matrix = get_matrix x_max y_max lines in
  let numbers = parse_numbers y_max matrix in
  let sum =
    Base.List.fold_left numbers ~init:0 ~f:(fun acc num ->
        if check_number (x_max - 1) (y_max - 1) matrix num then acc + num.num else acc)
  in
  string_of_int sum
