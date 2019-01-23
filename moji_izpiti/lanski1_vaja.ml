(* ===================== IZPIT 24.1.2019 ======================== *)

(* 1. NALOGA *)

(* a *)

let rec izpisi_vsa_stevila = function
  | [] -> ()
  | x :: xs -> 
  print_int x;
  izpisi_vsa_stevila xs 

(* b *)
let map2_opt f lst1 lst2 = 
  if List.length lst1 <> List.length lst2 then
    None
  else
  let rec map2_opt' acc lst1 lst2 =
    match lst1, lst2 with 
    | [], _ | _, [] -> List.rev acc
    | x :: xs, y :: ys -> map2_opt' ((f x y) :: acc) xs ys
  in (Some (map2_opt' [] lst1 lst2))
(* REŠITEV *)

let rec map2_opt l1 l2 f =
  let rec aux l1 l2 f acc =
    match l1, l2 with
    | [], [] -> Some (List.rev acc)
    | [], _ -> None
    | _, [] -> None
    | x::xs, y::ys ->
      let a = f x y in
      aux xs ys f (a::acc)
  in
  aux l1 l2 f []

(* 2. NALOGA *)

(* a *)

type filter_tree =
  | Node of filter_tree * int * filter_tree
  | Box of int list

let filter_tree = Node(
  Node(Box([1]), 5, Box([])), 
  10, 
  Node(Box([]), 15, Box([19; 20])))


(* b *)

let rec vstavi n = function
  | Box (xs) -> Box (n :: xs)
  | Node(lt, k, rt) ->
  if n <= k then
    Node(vstavi n lt, k, rt)
  else
    Node(lt, k, vstavi n rt)

(* c *)

let rec vstavi_seznam filter_tree = function
  | [] -> filter_tree
  | x :: xs -> vstavi_seznam (vstavi x filter_tree) xs


(* d *)
let rec min_of_list = function
     | [] -> failwith "List too short"
     | x :: [] -> x
     | x :: y :: [] -> if x <=y then x else y
     | x :: y :: xs -> if x <= y then min_of_list (x :: xs) else min_of_list (y :: xs)

let rec max_of_list = function
     | [] -> failwith "List too short"
     | x :: [] -> x
     | x :: y :: [] -> if x <=y then y else x
     | x :: y :: xs -> if x <= y then max_of_list (y :: xs) else max_of_list (x :: xs)

let rec check = function
  | Box (xs) -> true
  | Node(Box(xs), k, Box(ys)) -> (
    match xs, ys with
    | _, [] | [], _ -> true
    | x :: xs, y :: ys ->
      if ((max_of_list xs <= k) && (min_of_list ys > k)) then true else false
  )
  | Node(lt, k, rt) -> if (check lt && check rt) then true else false 

let test1 = Node(Box([1;2]), 5, Box([7]))
let test2 = Node(Box([1]), 5, Box([2;7]))
(* 3. NALOGA *)
type vektor = int * int
type matrika = int * int * int * int

module type Linearna = sig
  type t 
  val id : t 
  val uporabi : t -> int*int -> int*int
  val iz_matrike : int*int*int*int -> t 
  val iz_funkcije : (int*int -> int*int) -> t 
  val kompozitum : t -> t -> t 

end

(* a *)

module Matrika : Linearna = struct
  type t = int * int * int * int

  let id = (1,0,0,1)
  let uporabi (a, b, c, d) (x, y) = (a*x + b*y, c*x + d*y)
  let iz_matrike t = t
  let iz_funkcije f  = 
    let (a, c) = f (1, 0) in
    let (b, d) = f (0, 1) in
    (a, b, c, d)
  let kompozitum (a, b, c, d) (e, f, g, h) =
    (a*e + b*g, a*f + b*h, c*e + d*g, c*f + c*h)

end

(* b *)

module Funkcija : Linearna = struct
  type t = int*int -> int*int

  let id = (fun x -> x)
  let uporabi t vektor = t vektor
  let iz_matrike (a, b, c, d) = fun (x, y) -> (a*x + b*y, c*x + d * y)
  let iz_funkcije f = f
  let kompozitum t1 t2 = fun x -> t1 (t2 x)
end


(* 4.NALOGA *)
(* V pythonu. *)


    