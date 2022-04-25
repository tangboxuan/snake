open! Base

type t =
  { mutable snake : Snake.t
  ; mutable snake2 : Snake.t
  ; mutable game_state : Game_state.t
  ; mutable apple : Apple.t
  ; board : Board.t
  ; mutable score : int
  ; mutable score2 : int
  }
[@@deriving sexp_of, fields]

let to_string { snake; snake2; game_state; apple; board; score; score2 } =
  Core.sprintf
    !{|Game state: %{sexp:Game_state.t}
Apple: %{sexp:Apple.t}
Board: %{sexp:Board.t}
Snake:
%s |}
    game_state
    apple
    board
    (Snake.to_string ~indent:2 snake)
;;

let create ~height ~width ~initial_snake_length =
  let board = Board.create ~height ~width in
  let snake = Snake.create ~length:initial_snake_length ~no:1 in
  let snake2 = Snake.create ~length:initial_snake_length ~no:2 in
  let apple = Apple.create ~board ~snake ~snake2 in
  match apple with
  | None -> failwith "unable to create initial apple"
  | Some apple ->
    let t =
      { snake; snake2; apple; game_state = In_progress; board; score = 0; score2 = 0 }
    in
    if List.exists (Snake.all_locations snake) ~f:(fun pos ->
           not (Board.in_bounds t.board pos))
       || List.exists (Snake.all_locations snake2) ~f:(fun pos ->
              not (Board.in_bounds t.board pos))
    then failwith "unable to create initial snake"
    else t
;;

let snake t = t.snake
let snake2 t = t.snake2
let apple t = t.apple
let game_state t = t.game_state
let score t = t.score
let score2 t = t.score2

let handle_key t key =
  match Direction.of_key key with
  (* | Some (1, d) ->
    if not (Direction.opposites d (Snake.direction t.snake))
    then Snake.set_direction t.snake d
  | Some (2, d) ->
    if not (Direction.opposites d (Snake.direction t.snake2))
    then Snake.set_direction t.snake2 d *)
  | Some (1, d) -> Snake.set_direction t.snake d
  | Some (2, d) -> Snake.set_direction t.snake2 d
  | Some _ -> ()
  | None -> ()
;;

let check_for_collisions t =
  if not (Board.in_bounds t.board (Snake.head t.snake))
  then t.game_state <- Win2 "Snake 1 Out of bounds!"
  else if not (Board.in_bounds t.board (Snake.head t.snake2))
  then t.game_state <- Win1 "Snake 2 Out of bounds!"
  else if List.mem
            (Snake.all_locations t.snake2)
            (Snake.head t.snake)
            ~equal:Position.equal
  then t.game_state <- Win2 "Snake 1 went into Snake 2!"
  else if List.mem
            (Snake.all_locations t.snake)
            (Snake.head t.snake2)
            ~equal:Position.equal
  then t.game_state <- Win2 "Snake 2 went into Snake 1!"
;;

let maybe_consume_apple t =
  let consume_apple snake score set_score =
    Snake.grow_over_next_steps snake (Apple.amount_to_grow t.apple);
    match Apple.color t.apple with
    | Red -> set_score (score + 1)
    | Gold ->
      set_score (score + 2);
      (match Apple.create ~board:t.board ~snake:t.snake ~snake2:t.snake2 with
      | Some a -> t.apple <- a
      | None -> t.game_state <- Win)
  in
  if Position.equal (Snake.head t.snake) (Apple.location t.apple)
  then consume_apple t.snake t.score (fun score -> t.score <- score)
  else if Position.equal (Snake.head t.snake2) (Apple.location t.apple)
  then consume_apple t.snake2 t.score2 (fun score -> t.score2 <- score)
;;

Field.get

let step t =
  if Snake.step t.snake
  then (
    check_for_collisions t;
    maybe_consume_apple t;
    if Snake.step t.snake2
    then (
      check_for_collisions t;
      maybe_consume_apple t)
    else t.game_state <- Win1 "Snake2 collision!")
  else t.game_state <- Win2 "Snake1 collision!"
;;

module Exercises = struct
  let exercise02b = handle_key

  let exercise03b t snake =
    let t = { t with snake } in
    check_for_collisions t;
    t.game_state
  ;;

  let exercise04b t snake =
    let t = { t with snake } in
    step t;
    t.snake, t.game_state
  ;;

  let exercise06b = maybe_consume_apple
  let set_apple t apple = t.apple <- apple
  let set_snake t snake = t.snake <- snake
end
