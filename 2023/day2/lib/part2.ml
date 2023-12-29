open Core

type gameType = { mutable red : int; mutable blue : int; mutable green : int }
type gameIterationType = { num : int; games : gameType list }

let new_gametype red blue green = { red; blue; green }
let empty_gametype () = { red = 0; blue = 0; green = 0 }

let parse_game_num_string game_num_s =
  let _, num_string = Base.String.lsplit2_exn game_num_s ~on:' ' in
  int_of_string num_string

let parse_game_string (game_s : string) : gameType list =
  let parse_single_game single_game_s =
    let outcomes = Base.String.split single_game_s ~on:',' in
    Base.List.fold_left outcomes ~init:(empty_gametype ())
      ~f:(fun acc outcome ->
        let outcome = Base.String.strip outcome in
        let num_string, color = Base.String.lsplit2_exn outcome ~on:' ' in
        if Base.String.equal color "red" then
          new_gametype (int_of_string num_string) acc.blue acc.green
        else if Base.String.equal color "blue" then
          new_gametype acc.red (int_of_string num_string) acc.green
        else if Base.String.equal color "green" then
          new_gametype acc.red acc.blue (int_of_string num_string)
        else failwith (Printf.sprintf "Unknown color: %s" color))
  in
  Base.List.map (Base.String.split game_s ~on:';') ~f:(fun single_game_s ->
      parse_single_game single_game_s)

let parse_line line =
  let game_num_s, game_s = Base.String.lsplit2_exn line ~on:':' in
  { num = parse_game_num_string game_num_s; games = parse_game_string game_s }

let compute_sum powers =
  let rec _compute_sum acc = function
    | [] -> acc
    | p :: rest -> _compute_sum (acc + p) rest
  in
  _compute_sum 0 powers

let compute_power game_iteration =
  let compute_game_required acc game =
    if game.red > acc.red then acc.red <- game.red;
    if game.blue > acc.blue then acc.blue <- game.blue;
    if game.green > acc.green then acc.green <- game.green;
    acc
  in
  let base = empty_gametype () in
  let required =
    Base.List.fold_left game_iteration.games ~init:base ~f:(fun acc game ->
        compute_game_required acc game)
  in
  required.red * required.blue * required.green

let setup : gameType = { red = 12; blue = 14; green = 13 }

let solve f =
  let lines = In_channel.read_lines f in
  let games = Base.List.map lines ~f:(fun line -> parse_line line) in
  let powers = Base.List.map games ~f:(fun game -> compute_power game) in
  string_of_int (compute_sum powers)
