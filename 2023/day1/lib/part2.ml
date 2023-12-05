open Core

type parseSet = End of string | Node of (char, parseSet) Base.Hashtbl.t

let new_parseset_node () = Node (Base.Hashtbl.create (module Char))

let add_parseset_node k = function
  | End _ -> ()
  | Node ht -> (
      match Base.Hashtbl.find ht k with
      | None -> Base.Hashtbl.add_exn ht ~key:k ~data:(new_parseset_node ())
      | Some _ -> ())

let add_parseset_end full_string k = function
  | End _ -> failwith "Don't call with End"
  | Node ht -> Base.Hashtbl.add_exn ht ~key:k ~data:(End full_string)

let get_parseset k = function
  | End _ -> failwith "Don't call with End"
  | Node ps -> Base.Hashtbl.find ps k

let explode_string s = List.init (String.length s) ~f:(String.get s)

let parseset_string_of ps =
  let rec _recursion ps acc =
    match ps with
    | End s -> acc ^ s
    | Node ps ->
        Base.Hashtbl.fold ps ~init:acc ~f:(fun ~key ~data acc ->
            Char.to_string key ^ "->" ^ _recursion data acc)
  in
  _recursion ps ""

let generateParseSet s =
  let ps = new_parseset_node () in
  let rec _recursion ps full_string = function
    | [] -> ()
    | [ s ] -> add_parseset_end full_string s ps
    | s :: rest -> (
        add_parseset_node s ps;
        match get_parseset s ps with
        | None -> ()
        | Some pss -> _recursion pss full_string rest)
  in
  List.iter s ~f:(fun s ->
      let exploded = explode_string s in
      _recursion ps s exploded);
  ps

type resetTo = None | Some of char list

let parse ps s =
  let exploded_string = explode_string s in
  let acc : string list = [] in
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
            | End s -> _parse ps (s :: acc) None rest
            | pss -> (
                match _reset_to with
                | None -> _parse pss acc (Some rest) rest
                | rt -> _parse pss acc rt rest)))
  in

  _parse ps acc reset_to exploded_string

type sum = None | Some of int

let parsed_to_int parsed =
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
  in
  let words_to_int_map = Base.Map.of_alist_exn (module String) word_to_ints in
  let ints =
    List.fold_left parsed ~init:[] ~f:(fun acc s ->
        match Base.Map.find words_to_int_map s with
        | None -> failwith ("Invalid string: " ^ s)
        | Some k -> k :: acc)
  in
  if List.length ints = 1 then (List.hd_exn ints * 10) + List.hd_exn ints
  else
    let rec compute_sum (acc : sum) = function
      | [] -> None
      | [ x ] -> (
          match acc with
          | None -> failwith "Something went wrong"
          | Some y -> Some (x + (y * 10)))
      | x :: rest -> (
          match acc with
          | None -> compute_sum (Some x) rest
          | y -> compute_sum y rest)
    in
    match compute_sum None ints with
    | None -> failwith "something went wrong"
    | Some x -> x

let keySet =
  [
    "1";
    "2";
    "3";
    "4";
    "5";
    "6";
    "7";
    "8";
    "9";
    "one";
    "two";
    "three";
    "four";
    "five";
    "six";
    "seven";
    "eight";
    "nine";
  ]

let solve f =
  let lines = In_channel.read_lines f in
  let ps = generateParseSet keySet in
  let sum =
    List.fold_left lines ~init:0 ~f:(fun acc line ->
        let parsed = parse ps line in
        (* print_endline (string_of_int (parsed_to_int parsed)); *)
        acc + parsed_to_int parsed)
  in
  string_of_int sum
