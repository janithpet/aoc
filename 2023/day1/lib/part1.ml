open Core

let explode_string s = List.init (String.length s) ~f:(String.get s)
let is_digit = function '0' .. '9' -> true | _ -> false

type state = { first : int; last : int }

let empty_state () = { first = 0; last = 0 }
let set_state first last = { first; last }

let solve f =
  let lines = In_channel.read_lines f in
  let sum =
    List.fold_left lines ~init:0 ~f:(fun acc line ->
        let chars = explode_string line in
        let state =
          List.fold_left chars ~init:(empty_state ()) ~f:(fun (acc : state) c ->
              if is_digit c then
                let c_int = Base.Int.of_string (String.make 1 c) in
                if acc.first = 0 then set_state c_int 0
                else set_state acc.first c_int
              else acc)
        in
        if state.last = 0 then acc + (state.first * 10) + state.first
        else acc + (state.first * 10) + state.last)
  in
  string_of_int sum
