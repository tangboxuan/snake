open! Base

type t =
  | Left
  | Up
  | Right
  | Down
[@@deriving sexp_of]

let next_position t { Position.row; col } : Position.t =
  match t with
  | Left -> { row; col = col - 1 }
  | Right -> { row; col = col + 1 }
  | Up -> { row = row + 1; col }
  | Down -> { row = row - 1; col }
;;

let of_key key =
  match key with
  | 'w' -> Some (1, Up)
  | 'a' -> Some (1, Left)
  | 's' -> Some (1, Down)
  | 'd' -> Some (1, Right)
  | 'i' -> Some (2, Up)
  | 'j' -> Some (2, Left)
  | 'k' -> Some (2, Down)
  | 'l' -> Some (2, Right)
  | _ -> None
;;

let opposites a b =
  match a, b with
  | Left, Right -> true
  | Right, Left -> true
  | Up, Down -> true
  | Down, Up -> true
  | _ -> false
;;

module Exercises = struct
  let exercise02a = of_key
end
