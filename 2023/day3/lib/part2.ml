open Core

let x_size = 140
let y_size = 140

type coordinate = { x : int; y : int }

let new_coordinate x y = { x; y }
let string_of_coordinate coord = "(" ^ string_of_int coord.x ^ "," ^ string_of_int coord.y ^ ")"

type number = { value : int; used : bool; coords : coordinate list }
type element = Symbol | Number of number

let rec set_number_used matrix = function
  | [] -> ()
  | coord :: coords -> (
      match matrix.(coord.x).(coord.y) with
      | Symbol -> ()
      | Number num ->
          matrix.(coord.x).(coord.y) <- Number { num with used = true };
          set_number_used matrix coords)

let string_of_element = function Symbol -> "." | Number { value; _ } -> string_of_int value
let explode_string s = List.init (String.length s) ~f:(String.get s)

type star = Star of coordinate

let string_of_star = function Star coord -> string_of_coordinate coord

type accumulator = { stars : star list; stack : string option; start : coordinate option }

let new_accumulator stars stack start = { stars; stack; start }

let get_number_inclusive_coords start stop =
  let x = start.x in
  let rec get_coords x y_high coords current_y =
    if current_y <= y_high then
      get_coords x y_high (new_coordinate x current_y :: coords) (current_y + 1)
    else coords
  in
  get_coords x stop.y [] start.y

let get_star_exclusive_coords coord =
  let x_high = if coord.x > 0 then coord.x - 1 else coord.x in
  let x_low = if coord.x < x_size - 1 then coord.x + 1 else coord.x in
  let y_high = if coord.y < y_size - 1 then coord.y + 1 else coord.y in
  let y_low = if coord.y > 0 then coord.y - 1 else coord.y in
  let coords = [ new_coordinate coord.x y_low; new_coordinate coord.x y_high ] in
  let rec add_outer_coords x y_max coords current_y =
    if current_y <= y_max then
      add_outer_coords x y_max (new_coordinate x current_y :: coords) (current_y + 1)
    else coords
  in
  let coords = add_outer_coords x_low y_high coords y_low in
  add_outer_coords x_high y_high coords y_low

let update_matrix_with_number number matrix coords =
  let rec _update_matrix_with_number number matrix full_coords = function
    | [] -> ()
    | coord :: coords ->
        matrix.(coord.x).(coord.y) <- Number { value = number; used = false; coords = full_coords };
        _update_matrix_with_number number matrix full_coords coords
  in
  _update_matrix_with_number number matrix coords coords

let close_accumulator_stack accumulator current_coordinate matrix =
  match accumulator.stack with
  | None -> accumulator
  | Some number_string ->
      let num = int_of_string number_string in
      let start_coord = Option.value_exn accumulator.start in
      let coords = get_number_inclusive_coords start_coord current_coordinate in
      update_matrix_with_number num matrix coords;
      { accumulator with stack = None; start = None }

let add_to_stack c coord accumulator =
  match accumulator.stack with
  | None -> { accumulator with stack = Some (Char.to_string c); start = Some coord }
  | Some s -> { accumulator with stack = Some (s ^ Char.to_string c) }

let rec print_stars = function
  | [] -> ()
  | star :: stars ->
      print_endline (string_of_star star);
      print_stars stars

let print_matrix matrix =
  Base.Array.iteri matrix ~f:(fun x row ->
      Base.Array.iteri row ~f:(fun y ele ->
          match ele with
          | Symbol -> ()
          | Number { value; _ } ->
              print_endline (string_of_int value ^ ":" ^ string_of_coordinate (new_coordinate x y))))

let rec check_if_gear_and_sum acc matrix = function
  | [] -> acc
  | Star coord :: stars ->
      let check_coordinates = get_star_exclusive_coords coord in
      (* let rec print_coords = function
           | [] -> print_endline ""
           | coord :: coords ->
               print_endline (string_of_coordinate coord);
               print_coords coords
         in *)
      (* print_coords check_coordinates; *)
      let gear_ratio, count =
        Base.List.fold check_coordinates ~init:(1, 0) ~f:(fun acc coord ->
            let gear_ratio, count = acc in
            match matrix.(coord.x).(coord.y) with
            | Symbol -> (gear_ratio, count)
            | Number { value; used; coords } ->
                if not used then (
                  set_number_used matrix coords;
                  (gear_ratio * value, count + 1))
                else (gear_ratio, count))
      in
      let acc = if count = 2 then acc + gear_ratio else acc in
      check_if_gear_and_sum acc matrix stars

let solve f =
  let lines = In_channel.read_lines f in
  let matrix = Base.Array.make_matrix ~dimx:x_size ~dimy:y_size Symbol in
  let accumulator =
    Base.List.foldi lines ~init:(new_accumulator [] None None) ~f:(fun x accumulator line ->
        let cs = explode_string line in
        let accumulator =
          Base.List.foldi cs ~init:accumulator ~f:(fun y accumulator c ->
              match c with
              | '0' .. '9' -> add_to_stack c (new_coordinate x y) accumulator
              | '*' ->
                  let accumulator =
                    close_accumulator_stack accumulator (new_coordinate x (y - 1)) matrix
                  in
                  { accumulator with stars = Star (new_coordinate x y) :: accumulator.stars }
              | _ -> close_accumulator_stack accumulator (new_coordinate x (y - 1)) matrix)
        in
        close_accumulator_stack accumulator (new_coordinate x (y_size - 1)) matrix)
  in
  let sum = check_if_gear_and_sum 0 matrix accumulator.stars in
  (* print_matrix matrix; *)
  (* print_endline "........."; *)
  (* print_stars accumulator.stars; *)
  string_of_int sum
