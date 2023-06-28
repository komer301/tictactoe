open! Core
open Tic_tac_toe_2023_common
open Protocol

(* Exercise 1.2.

   Implement a game AI that just picks a random available position. Feel free
   to raise if there is not an available position.

   After you are done, update [compute_next_move] to use your
   [random_move_strategy]. *)
let random_move_strategy
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let avail_spots =
    Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces
  in
  List.random_element_exn avail_spots
;;

(* Exercise 3.2.

   Implement a game AI that picks a random position, unless there is an
   available winning move.

   After you are done, update [compute_next_move] to use your
   [pick_winning_move_if_possible_strategy]. *)
let pick_winning_move_if_possible_strategy
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let winable =
    Tic_tac_toe_exercises_lib.winning_moves ~me ~game_kind ~pieces
  in
  if not (List.is_empty winable)
  then List.random_element_exn winable
  else random_move_strategy ~game_kind ~pieces
;;

(* disables unused warning. Feel free to delete once it's used. *)
let _ = pick_winning_move_if_possible_strategy

(* Exercise 4.2.

   Implement a game AI that picks a random position, unless there is an
   available winning move.

   After you are done, update [compute_next_move] to use your
   [pick_winning_move_if_possible_strategy]. *)
let pick_winning_move_or_block_if_possible_strategy
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let winable =
    Tic_tac_toe_exercises_lib.winning_moves ~me ~game_kind ~pieces
  in
  let loseable =
    Tic_tac_toe_exercises_lib.losing_moves ~me ~game_kind ~pieces
  in
  if not (List.is_empty winable)
  then List.random_element_exn winable
  else if not (List.is_empty loseable)
  then List.random_element_exn loseable
  else random_move_strategy ~game_kind ~pieces
;;

let score
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : float
  =
  match Tic_tac_toe_exercises_lib.evaluate ~game_kind ~pieces with
  | Tic_tac_toe_exercises_lib.Evaluation.Game_over { winner = Some winner }
    ->
    if Piece.equal winner me then Float.infinity else Float.neg_infinity
  | _ -> 0.0
;;

let _ = score

let minimax ~(me: Piece.t) ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t) ~(depth: int) ~(maximizing_player : bool): float =
  (* let g_o = match Tic_tac_toe_exercises_lib.evaluate ~game_kind ~pieces with
  | Tic_tac_toe_exercises_lib.Evaluation.Game_over {winner = Some winner} -> true
  | _ -> false
in
  if depth == 0 || g_o then
    score ~me ~game_kind ~pieces else
  in *)
  let avail = Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces in
  if (List.is_empty avail) || depth == 0 then 
    score ~me ~game_kind ~pieces else
  if maximizing_player then
    let value = Float.neg_infinity in
    List.iter avail ~f:(fun x -> Float.max value (minimax ~game_kind ~pieces:(Map.set pieces ~key:x ~data:me) ~depth:depth -1 ~maximizing_player: false) 
  else 
    let value = Float.infinity in 
    List.iter avail ~f:(fun)

  ;;
(* [compute_next_move] is your Game AI's function.

   [game_ai.exe] will connect, communicate, and play with the game server,
   and will use [compute_next_move] to pick which pieces to put on your
   behalf.

   [compute_next_move] is only called whenever it is your turn, the game
   isn't yet over, so feel free to raise in cases where there are no
   available spots to pick. *)
let compute_next_move ~(me : Piece.t) ~(game_state : Game_state.t)
  : Position.t
  =
  pick_winning_move_or_block_if_possible_strategy
    ~me
    ~game_kind:game_state.game_kind
    ~pieces:game_state.pieces
;;
