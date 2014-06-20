(* 

  An OCaml implementation of 2048, inspired by 
  http://gregorulm.com/2048-in-90-lines-haskell/

  Frank Cangialosi

*)

open List;;
open Random;;

(* initialize random number generator*)
self_init () ;;

(* Data type specifying direction of grid movement*)
type direction = Left | Right | Up | Down;;

(* -------------------------- *) 
(*  List manipulation helpers *)
(* -------------------------- *) 
let repeat x n = 
  let rec aux acc ns = 
    if ns > 0 then aux (x::acc) (ns-1) else acc in
  aux [] n
;;

let pad row = row@(repeat 0 (4 - (List.length row)));;

let clean row =
  let rec aux acc lst = match lst with
  | [] -> acc 
  | (h::t) -> 
    if (h != 0) then aux (h::acc) t 
    else aux acc t
  in rev (aux [] row)
;; 

let span p xs = 
  let (s, r) = 
    let rec aux acc = function 
      | [] -> (acc,[])
      | (h::t) -> if ((p h) && (length acc < 1)) then aux (h::acc) t else (acc,(h::t))
    in aux [] xs 
  in (rev s, r)
;;

let rec group = function
  | [] -> []
  | (h::t) -> let (ys,zs) = span (fun x -> x == h) t in (h::ys)::group zs
;;

let add = function
  | [] -> 0
  | [x] -> x
  | (x::y::xs) -> (x+y)
;;

let rec transpose grid =
  if mem [] grid then
    []
  else 
    (map hd grid) :: transpose (map tl grid)
;;

(* ------------------------- *)
(* Grid  helpers *)
(* ------------------------- *)

(* Shifts a single row to the left *)
let shift row = 
  pad (map add (group (clean row)))
;;

(* Shfit entire grid by direction d *)
let rec move d grid = match d with
  | Left -> (map shift grid)
  | Right -> map rev (map shift (map rev grid))
  | Up -> transpose (move Left (transpose grid))
  | Down -> transpose (move Right (transpose grid))
;;

(* change element n to x in lst *)
let change_nth n x lst =
  let rec aux ns acc = function
    | [] -> acc 
    | (h::t) -> if (ns==0) then aux (ns-1) (x::acc) t else aux (ns-1) (h::acc) t 
  in rev (aux n [] lst)
;;

(* --------------- *)
(* I/O and helpers *)
(* --------------- *)

let row_to_string row = 
  let rec aux = function 
    | [] -> ""
    | [h] -> string_of_int h
    | (h::t) -> string_of_int h ^ " " ^ aux t
  in "[" ^ aux row ^ "]"
;;

let rec grid_to_string = function
    | [] -> ""
    | [h] -> row_to_string h
    | (h::t) -> row_to_string h ^ "\n" ^ grid_to_string t
;;

let print_grid grid = print_endline (grid_to_string grid)
;;

let string_to_dir = function
  | "a" -> Left
  | "d" -> Right
  | "s" -> Down
  | "w" -> Up
  | _ -> Left (* default to left if input malformed *)
;;

(* ------------------- *)
(* Game Flow Functions *)
(* ------------------- *)

(* return the grid with one empty space changed to a 2 *)
let new_tile grid = 
  let (rand_row,rand_pos) = (int 4, int 4) in 
  let row = nth grid rand_row in 
  let new_row = change_nth rand_pos 2 row in
  change_nth rand_row new_row grid
;;

(* if one of the tiles is 2048, then the player has won the game *)
let won grid = 
  mem true (map (mem 2048) grid)
;; 

let lost grid = 
  not (mem true (map (fun dir -> (grid <> (move dir grid))) [Left;Right;Up;Down]))
;;

(* --------- *)
(* Game Loop *)
(* --------- *)
let start_grid = [[0;0;0;0];
                  [0;0;0;2];
                  [0;0;0;2];
                  [0;0;0;0]];;

let rec run grid =
  print_grid grid;
  if lost grid then
    print_endline "Game over!"
  else if won grid then
    print_endline "You won!"
  else begin 
    let line = input_line stdin in
    let dir = string_to_dir line in
    let new_grid = move dir grid in
    run (new_tile new_grid)
  end
;;

run start_grid;;