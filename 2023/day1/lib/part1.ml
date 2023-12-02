open Core

let explode_string s = List.init (String.length s) ~f:(String.get s)
let is_digit = function '0' .. '9' -> true | _ -> false

type value = None | Digit of int
type state = { first : value; last : value }

let empty_state () = { first = None; last = None }
let new_state first last = { first; last }

let solve f =
  let lines = In_channel.read_lines f in
  let sum =
    List.fold_left lines ~init:0 ~f:(fun acc line ->
        let chars = explode_string line in
        let state =
          List.fold_left chars ~init:(empty_state ()) ~f:(fun (acc : state) c ->
              if is_digit c then
                let c_int = Base.Int.of_string (String.make 1 c) in
                match acc.first with
                | None -> new_state (Digit c_int) None
                | Digit _ -> new_state acc.first (Digit c_int)
              else acc)
        in
        match state.last with
        | None -> (
            match state.first with
            | None -> acc
            | Digit fst -> acc + (fst * 10) + fst)
        | Digit lst -> (
            match state.first with
            | None -> acc
            | Digit fst -> acc + (fst * 10) + lst))
  in
  string_of_int sum
