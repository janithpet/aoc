open Core

type parseSet =
  | End of int
  | Continue of int
  | Node of (char, parseSet) Base.Hashtbl.t

let new_parseset_node () = Node (Base.Hashtbl.create (module Char))

let add_parseset_node k = function
  | End _ -> ()
  | Continue _ -> ()
  | Node ht -> (
      match Base.Hashtbl.find ht k with
      | None -> Base.Hashtbl.add_exn ht ~key:k ~data:(new_parseset_node ())
      | Some _ -> ())

let add_parseset_end v k = function
  | End _ -> failwith "Don't call with End"
  | Continue _ -> failwith "Don't call with Continue"
  | Node ht -> Base.Hashtbl.add_exn ht ~key:k ~data:(End v)

let add_parseset_continue v k = function
  | End _ -> failwith "Don't call with End"
  | Continue _ -> failwith "Don't call with Continue"
  | Node ht -> Base.Hashtbl.add_exn ht ~key:k ~data:(Continue v)

let get_parseset k = function
  | End _ -> failwith "Don't call with End"
  | Continue _ -> failwith "Don't call with Continue"
  | Node ps -> Base.Hashtbl.find ps k

let explode_string s = List.init (String.length s) ~f:(String.get s)

let parseset_string_of ps =
  let rec _recursion ps acc =
    match ps with
    | End s -> acc ^ string_of_int s
    | Continue s -> acc ^ string_of_int s
    | Node ps ->
        Base.Hashtbl.fold ps ~init:acc ~f:(fun ~key ~data acc ->
            Char.to_string key ^ "->" ^ _recursion data acc)
  in
  _recursion ps ""

let generateParseSet s =
  let ps = new_parseset_node () in
  let rec _recursion ps v = function
    | [] -> ()
    | [ s ] -> add_parseset_continue v s ps
    | s :: rest -> (
        add_parseset_node s ps;
        match get_parseset s ps with
        | None -> ()
        | Some pss -> _recursion pss v rest)
  in
  Base.Map.iteri s ~f:(fun ~key:s ~data:v ->
      let exploded = explode_string s in
      if List.length exploded = 1 then
        add_parseset_end v (Base.List.hd_exn exploded) ps
      else _recursion ps v exploded);
  ps

type resetTo = None | Some of char list

let parse ps s =
  let exploded_string = explode_string s in
  let acc : int list = [] in
  let reset_to : resetTo = None in
  let rec _parse _ps acc (_reset_to : resetTo) = function
    | [] -> acc
    | s :: rest -> (
        match get_parseset s _ps with
        | None -> (
            match _reset_to with
            | None -> _parse ps acc None rest
            | Some rt -> _parse ps acc None rt)
        | Some pss -> (
            match pss with
            | End ss -> _parse ps (ss :: acc) None rest
            | Continue ss -> _parse ps (ss :: acc) None (s :: rest)
            | Node pss -> (
                match _reset_to with
                | None -> _parse (Node pss) acc (Some rest) rest
                | rt -> _parse (Node pss) acc rt rest)))
  in

  _parse ps acc reset_to exploded_string

type sum = None | Some of int

let word_to_ints =
  [
    ("one", 1);
    ("two", 2);
    ("three", 3);
    ("four", 4);
    ("five", 5);
    ("six", 6);
    ("seven", 7);
    ("eight", 8);
    ("nine", 9);
    ("1", 1);
    ("2", 2);
    ("3", 3);
    ("4", 4);
    ("5", 5);
    ("6", 6);
    ("7", 7);
    ("8", 8);
    ("9", 9);
  ]

let parse_key_set = Base.Map.of_alist_exn (module String) word_to_ints

let parsed_to_int parsed_ints =
  if List.length parsed_ints = 1 then
    (List.hd_exn parsed_ints * 10) + List.hd_exn parsed_ints
  else
    let rec compute_sum (acc : sum) = function
      | [] -> None
      | [ x ] -> (
          match acc with
          | None -> failwith "Something went wrong"
          | Some y -> Some (y + (x * 10)))
      | x :: rest -> (
          match acc with
          | None -> compute_sum (Some x) rest
          | y -> compute_sum y rest)
    in
    match compute_sum None parsed_ints with
    | None -> failwith "something went wrong"
    | Some x -> x

let solve f =
  let lines = In_channel.read_lines f in
  let ps = generateParseSet parse_key_set in
  let sum =
    List.fold_left lines ~init:0 ~f:(fun acc line ->
        let parsed = parse ps line in
        acc + parsed_to_int parsed)
  in
  string_of_int sum
