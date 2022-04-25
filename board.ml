open! Base

type t =
  { height : int
  ; width : int
  }
[@@deriving sexp, fields]

let create ~height ~width = { height; width }
let create_unlabeled height width = { height; width }

let in_bounds t { Position.row; col } =
  row >= 0 && row < t.height && col >= 0 && col < t.width
;;

let all_locations t =
  List.concat_map (List.range 0 t.height) ~f:(fun row ->
      List.map (List.range 0 t.width) ~f:(fun col -> { Position.row; col }))
;;

module Exercises = struct
  let exercise03a = in_bounds
end
