open Core

type gameType = { red : int; blue : int; green : int }
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

let check_game_validity setup = function
  | { red; blue; green } ->
      let red_match = red <= setup.red in
      let blue_match = blue <= setup.blue in
      let green_match = green <= setup.green in
      red_match && blue_match && green_match

let check_game_iteration_validity setup gameIteration =
  Base.List.fold_left gameIteration.games ~init:true ~f:(fun acc game ->
      check_game_validity setup game && acc)

let compute_sums (bit_map : bool list) (games : gameIterationType list) =
  let combined = Base.List.zip_exn bit_map games in
  let rec _compute_sums acc combined =
    match combined with
    | [] -> acc
    | (m, g) :: tl ->
        if m then _compute_sums (acc + g.num) tl else _compute_sums acc tl
  in
  _compute_sums 0 combined

let setup : gameType = { red = 12; blue = 14; green = 13 }

let solve f =
  let lines = In_channel.read_lines f in
  let games = Base.List.map lines ~f:(fun line -> parse_line line) in
  let bit_map =
    Base.List.map games ~f:(fun game ->
        check_game_iteration_validity setup game)
  in
  string_of_int (compute_sums bit_map games)
