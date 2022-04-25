open! Base

type t =
  | In_progress
  | Win
  | Win1 of string
  | Win2 of string
[@@deriving sexp_of, compare]

let to_string t =
  match t with
  | In_progress -> ""
  | Win -> "BOTH WIN!!"
  | Win1 x -> "PLAYER 1 WIN! " ^ x
  | Win2 x -> "PLAYER 2 WIN!" ^ x
;;
