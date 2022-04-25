open! Base

module Color = struct
  type t =
    | Red
    | Gold
  [@@deriving sexp_of, enumerate]

  let random () = List.random_element_exn all
end

type t =
  { location : Position.t
  ; colour : Color.t
  }
[@@deriving sexp_of]

let location t = t.location
let color t = t.colour

let amount_to_grow t =
  match color t with
  | Red -> 2
  | Gold -> 5
;;

let create ~board ~snake ~snake2 =
  let pos =
    List.filter (Board.all_locations board) ~f:(fun x ->
        not
          (List.mem
             (Snake.all_locations snake @ Snake.all_locations snake2)
             x
             ~equal:Position.equal))
  in
  (* Option.map (List.random_element pos) ~f:(fun p ->
      { location = p; colour = Color.random () }) *)
  let%map.Option p = List.random_element pos in
  { location = p; colour = Color.random () }
;;

module Exercises = struct
  let exercise05 = create
  let create_with_location location = { location; colour = Color.Red }
end
