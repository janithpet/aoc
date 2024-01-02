open Core

let explode_string s = List.init (String.length s) ~f:(String.get s)
let is_digit = function '0' .. '9' -> true | _ -> false
let is_ws = function ' ' -> true | _ -> false

type parser_result = { card : int list; winnings : int list; ours : int list }

let init_parser_result () = { card = []; winnings = []; ours = [] }

let string_of_int_list xs =
  let print_string = "(" in
  let rec _string acc = function
    | [] -> acc ^ ")"
    | [ x ] -> _string (acc ^ string_of_int x) []
    | x :: xs -> _string (acc ^ string_of_int x ^ ", ") xs
  in
  _string print_string xs

let print_parser_result parser_result =
  print_endline ("Card:\t\t" ^ string_of_int_list parser_result.card);
  print_endline ("Winnings:\t" ^ string_of_int_list parser_result.winnings);
  print_endline ("Ours:\t\t" ^ string_of_int_list parser_result.ours)

type 'a switching_parser = {
  switch_on : char option;
  stack : string option;
  parsed : parser_result;
  parse : char -> 'a switching_parser -> 'a switching_parser;
  on_switch : 'a switching_parser -> 'a switching_parser;
  next : parser_result -> 'a switching_parser option;
}

let init_parser switch_on parsed parse on_switch next =
  { switch_on; stack = None; parsed; parse; on_switch; next }

let close_stack acc convert_to_t = function None -> acc | Some s -> convert_to_t s :: acc

let add_to_stack c = function
  | None -> Some (Char.to_string c)
  | Some s -> Some (s ^ Char.to_string c)

let card_parse c t = if is_digit c then { t with stack = add_to_stack c t.stack } else t

let card_on_switch t =
  let card = close_stack t.parsed.card int_of_string t.stack in
  { t with stack = None; parsed = { t.parsed with card } }

let winnings_parse c t =
  if is_ws c then
    let winnings = close_stack t.parsed.winnings int_of_string t.stack in
    { t with stack = None; parsed = { t.parsed with winnings } }
  else if is_digit c then { t with stack = add_to_stack c t.stack }
  else t

let winnings_on_switch t =
  let winnings = close_stack t.parsed.winnings int_of_string t.stack in
  { t with stack = None; parsed = { t.parsed with winnings } }

let ours_parse c t =
  if is_ws c then
    let ours = close_stack t.parsed.ours int_of_string t.stack in
    { t with stack = None; parsed = { t.parsed with ours } }
  else if is_digit c then { t with stack = add_to_stack c t.stack }
  else t

let ours_on_switch t =
  let ours = close_stack t.parsed.ours int_of_string t.stack in
  { t with stack = None; parsed = { t.parsed with ours } }

let ours_parser parser_result =
  init_parser None parser_result ours_parse ours_on_switch (fun _ -> None)

let winnings_parser parser_result =
  init_parser (Some '|') parser_result winnings_parse winnings_on_switch (fun parser_result ->
      Some (ours_parser parser_result))

let card_parser parser_result =
  init_parser (Some ':') parser_result card_parse card_on_switch (fun parser_result ->
      Some (winnings_parser parser_result))

let run cs parser =
  let check_switch current_char switch_on = Base.Char.equal current_char switch_on in
  let parser =
    Base.List.fold cs ~init:parser ~f:(fun parser c ->
        let parser =
          match parser.switch_on with
          | None -> parser
          | Some switch_on ->
              if check_switch c switch_on then
                let parser = parser.on_switch parser in
                Option.value_exn (parser.next parser.parsed)
              else parser
        in
        parser.parse c parser)
  in
  parser.on_switch parser

let count_winnings { card = _; winnings; ours } =
  Base.List.fold winnings ~init:0 ~f:(fun acc winning ->
      if Base.List.exists ours ~f:(fun a -> a = winning) then acc + 1 else acc)

module Card = struct
  type t = Card of int

  let hash (Card a) = a
  let compare (Card a) (Card b) = Base.Int.compare a b
  let sexp_of_t (Card a) = Base.Int.sexp_of_t a
end

let new_card number number_of_wins (ds : (Card.t, Card.t list) Base.Hashtbl.t) =
  let open Card in
  let rec add_cards_won acc to_add = function
    | 0 -> acc
    | v -> add_cards_won (Card to_add :: acc) (to_add + 1) (v - 1)
  in
  let cards_won = add_cards_won [] (number + 1) number_of_wins in
  Hashtbl.add_exn ds ~key:(Card number) ~data:cards_won;
  Card number

let num_lines = 206

let solve f =
  let lines = In_channel.read_lines f in
  let ds = Base.Hashtbl.create ~growth_allowed:false ~size:num_lines (module Card) in
  let cards =
    Base.List.fold lines ~init:[] ~f:(fun acc line ->
        let cs = explode_string line in
        let parser = run cs (card_parser (init_parser_result ())) in
        let number_of_wins = count_winnings parser.parsed in
        let card = new_card (Option.value_exn (List.hd parser.parsed.card)) number_of_wins ds in
        card :: acc)
  in
  let rec traverse acc card =
    let cards_won = Base.Hashtbl.find_exn ds card in
    Base.List.fold cards_won ~init:acc ~f:(fun acc card -> traverse (acc + 1) card)
  in
  let sum =
    Base.List.fold cards ~init:0 ~f:(fun acc card ->
        let acc = acc + 1 in
        traverse acc card)
  in
  string_of_int sum
