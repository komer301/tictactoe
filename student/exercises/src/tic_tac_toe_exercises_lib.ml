open Core
open Tic_tac_toe_2023_common
open Protocol

module Evaluation = struct
  type t =
    | Illegal_state
    | Game_over of { winner : Piece.t option }
    | Game_continues
  [@@deriving sexp_of]

  let to_string (t : t) = t |> sexp_of_t |> Sexp.to_string
end

(* Here are some functions which know how to create a couple different kinds
   of games *)
let empty_game =
  let game_id = Game_id.of_int 0 in
  let game_kind = Game_kind.Tic_tac_toe in
  let player_x = Player.Player (Username.of_string "Player_X") in
  let player_o = Player.Player (Username.of_string "Player_O") in
  let game_status = Game_status.Turn_of Piece.X in
  { Game_state.game_id
  ; game_kind
  ; player_x
  ; player_o
  ; pieces = Position.Map.empty
  ; game_status
  }
;;

let place_piece (game : Game_state.t) ~piece ~position : Game_state.t =
  let pieces = Map.set game.pieces ~key:position ~data:piece in
  { game with pieces }
;;


let win_for_x =
  empty_game
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
;;

let non_win =
  empty_game
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
;;

(* Exercise 1.

   For instructions on implemeting this refer to the README.

   After you are done with this implementation, you can uncomment out
   "evaluate" test cases found below in this file. *)
let all_spots ~(game_kind : Game_kind.t) =
  let length = Protocol.Game_kind.board_length game_kind in
  let all_chords =
    List.init length ~f:(fun x ->
      List.init length ~f:(fun y -> { Position.row = x; column = y }))
  in
  let concat_coord = List.concat all_chords in
  let set_all_coords = Position.Set.of_list concat_coord in
  set_all_coords
;;

let available_moves
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let keys = Map.keys pieces in
  let set_of_keys = Position.Set.of_list keys in
  let all_slots = all_spots ~game_kind in
  let avail_pos = Set.diff all_slots set_of_keys in
  Set.to_list avail_pos
;;

let starting ~(game_kind : Game_kind.t) =
  let length = Protocol.Game_kind.board_length game_kind in
  let rows =
    List.init length ~f:(fun x -> { Position.row = x; column = 0 })
  in
  let cols =
    List.init length ~f:(fun x -> { Position.row = 0; column = x })
  in
  cols, rows
;;

let rec row
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  ~(start : Position.t)
  ~(length : int)
  : int
  =
  if length = 0
  then 0
  else (
    let value =
      match Map.find pieces start with
      | Some X -> 1
      | Some O -> -1
      | None -> 0
    in
    let next =
      row
        ~game_kind
        ~pieces
        ~start:(Position.right start)
        ~length:(length - 1)
    in
    value + next)
;;

let rec col
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  ~(start : Position.t)
  ~(length : int)
  : int
  =
  if length = 0
  then 0
  else (
    let value =
      match Map.find pieces start with
      | Some X -> 1
      | Some O -> -1
      | None -> 0
    in
    let next =
      col ~game_kind ~pieces ~start:(Position.down start) ~length:(length - 1)
    in
    value + next)
;;

let rec diag_left
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  ~(start : Position.t)
  ~(length : int)
  : int
  =
  if length = 0
  then 0
  else (
    let value =
      match Map.find pieces start with
      | Some X -> 1
      | Some O -> -1
      | None -> 0
    in
    let next =
      diag_left
        ~game_kind
        ~pieces
        ~start:(Position.right (Position.down start))
        ~length:(length - 1)
    in
    value + next)
;;

let rec diag_right
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  ~(start : Position.t)
  ~(length : int)
  : int
  =
  if length = 0
  then 0
  else (
    let value =
      match Map.find pieces start with
      | Some X -> 1
      | Some O -> -1
      | None -> 0
    in
    let next =
      diag_right
        ~game_kind
        ~pieces
        ~start:(Position.left (Position.down start))
        ~length:(length - 1)
    in
    value + next)
;;

(* Exercise 2.

   For instructions on implemeting this refer to the README.

   After you are done with this implementation, you can uncomment out
   "evaluate" test cases found below in this file. *)
let evaluate ~(game_kind : Game_kind.t) ~(pieces : Piece.t Position.Map.t)
  : Evaluation.t
  =
  let avail = available_moves ~game_kind ~pieces in
  let length = Protocol.Game_kind.board_length game_kind in
  let find_col, find_row = starting ~game_kind:(game_kind : Game_kind.t) in
  let row_values =
    List.map find_row ~f:(fun x -> row ~game_kind ~pieces ~start:x ~length)
  in
  let col_values =
    List.map find_col ~f:(fun y -> col ~game_kind ~pieces ~start:y ~length)
  in
  let diag_values_left =
    diag_left ~game_kind ~pieces ~start:{ row = 0; column = 0 } ~length
  in
  let diag_values_right =
    diag_right
      ~game_kind
      ~pieces
      ~start:{ row = 0; column = length - 1 }
      ~length
  in
  let checking_o =
    List.exists row_values ~f:(Int.equal (-3))
    || List.exists col_values ~f:(Int.equal (-3))
    || equal diag_values_left (-3)
    || equal diag_values_right (-3)
  in
  let checking_x =
    List.exists row_values ~f:(Int.equal 3)
    || List.exists col_values ~f:(Int.equal 3)
    || equal diag_values_left 3
    || equal diag_values_right 3
  in
  if checking_o && checking_x
  then Evaluation.Illegal_state
  else if checking_o
  then Evaluation.Game_over { winner = Some O }
  else if checking_x
  then Evaluation.Game_over { winner = Some X }
  else if List.is_empty avail
  then Evaluation.Game_over { winner = None }
  else Evaluation.Game_continues
;;

(* Exercise 3. *)
let winning_moves
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  (* print_s [%message "" (me : Piece.t)]; *)
  let available_spots = available_moves ~game_kind ~pieces in
  List.filter available_spots ~f:(fun x ->
    match evaluate ~game_kind ~pieces:(Map.set pieces ~key:x ~data:me) with
    | Evaluation.Game_over { winner = Some winner } -> Piece.equal winner me
    | _ -> false)
;;

(* Exercise 4. *)
let losing_moves
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  winning_moves ~me:(Piece.flip me) ~game_kind ~pieces
;;
let place_piece_ai (game : Game_state.t) ~piece ~position : Game_state.t =
  let pieces = Map.set game.pieces ~key:position ~data:piece in
  let game_eval = evaluate ~game_kind:game.game_kind ~pieces:pieces in 
  { game with pieces ; game_eval}
;;

let exercise_one =
  Command.basic
    ~summary:"Exercise 1: Where can I move?"
    (let%map_open.Command () = return () in
     fun () ->
       let moves =
         available_moves
           ~game_kind:win_for_x.game_kind
           ~pieces:win_for_x.pieces
       in
       print_s [%sexp (moves : Position.t list)];
       let moves =
         available_moves ~game_kind:non_win.game_kind ~pieces:non_win.pieces
       in
       print_s [%sexp (moves : Position.t list)])
;;

let exercise_two =
  Command.basic
    ~summary:"Exercise 2: Did is the game over?"
    (let%map_open.Command () = return () in
     fun () ->
       let evaluation =
         evaluate ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces
       in
       print_s [%sexp (evaluation : Evaluation.t)])
;;

let exercise_three =
  let piece_options =
    Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "
  in
  Command.basic
    ~summary:"Exercise 3: Is there a winning move?"
    (let%map_open.Command () = return ()
     and piece =
       flag
         "piece"
         (required (Arg_type.create Piece.of_string))
         ~doc:("PIECE " ^ piece_options)
     in
     fun () ->
       let winning_moves =
         winning_moves
           ~me:piece
           ~game_kind:non_win.game_kind
           ~pieces:non_win.pieces
       in
       print_s [%sexp (winning_moves : Position.t list)];
       ())
;;

let exercise_four =
  let piece_options =
    Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "
  in
  Command.basic
    ~summary:"Exercise 4: Is there a losing move?"
    (let%map_open.Command () = return ()
     and piece =
       flag
         "piece"
         (required (Arg_type.create Piece.of_string))
         ~doc:("PIECE " ^ piece_options)
     in
     fun () ->
       let losing_moves =
         losing_moves
           ~me:piece
           ~game_kind:non_win.game_kind
           ~pieces:non_win.pieces
       in
       print_s [%sexp (losing_moves : Position.t list)];
       ())
;;

let%expect_test "print_win_for_x" =
  print_endline (Game_state.to_string_hum win_for_x);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    XOX
    OOX
    OXX |}]
;;

let%expect_test "print_non_win" =
  print_endline (Game_state.to_string_hum non_win);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    X
    O
    O X |}]
;;

(* After you've implemented [available_moves], uncomment these tests! *)
(* let%expect_test "yes available_moves" = let (moves : Position.t list) =
   available_moves ~game_kind:non_win.game_kind ~pieces:non_win.pieces |>
   List.sort ~compare:Position.compare in print_s [%sexp (moves : Position.t
   list)]; [%expect {| (((row 0) (column 1)) ((row 0) (column 2)) ((row 1)
   (column 1)) ((row 1) (column 2)) ((row 2) (column 1))) |}] ;; *)

let%expect_test "no available_moves" =
  let (moves : Position.t list) =
    available_moves ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces
    |> List.sort ~compare:Position.compare
  in
  print_s [%sexp (moves : Position.t list)];
  [%expect {| () |}]
;;

(* When you've implemented the [evaluate] function, uncomment the next two
   tests! *)
let%expect_test "evalulate_win_for_x" =
  print_endline
    (evaluate ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces
     |> Evaluation.to_string);
  [%expect {| (Game_over(winner(X))) |}]
;;

let%expect_test "evalulate_non_win" =
  print_endline
    (evaluate ~game_kind:non_win.game_kind ~pieces:non_win.pieces
     |> Evaluation.to_string);
  [%expect {| Game_continues |}]
;;

(* When you've implemented the [winning_moves] function, uncomment this
   test! *)
let%expect_test "winning_move" =
  let positions =
    winning_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.X
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| ((((row 1) (column 1))))
  |}];
  let positions =
    winning_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.O
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| () |}]
;;

(* When you've implemented the [losing_moves] function, uncomment this
   test! *)
let%expect_test "print_losing" =
  let positions =
    losing_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.X
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| () |}];
  let positions =
    losing_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.O
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect
    {|
  ((((row 0) (column 1)) ((row 0) (column 2)) ((row 1) (column 2)) ((row 2)
  (column 1)))) |}]
;;
