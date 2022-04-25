open! Base

(** A [t] represents the current state of the game. *)
type t =
  | In_progress
  | Win
  | Win1 of string
  | Win2 of string
[@@deriving sexp_of, compare]

(** [to_string] pretty-prints the current game state into a string. *)
val to_string : t -> string
