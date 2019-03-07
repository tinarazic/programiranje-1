(* ===================== IZPIT 24.1.2019 ======================== *)

(* 1. NALOGA *)

(* a *)

let podvoji_vsoto a b = 2*(a+b)

(* b *)

let rec povsod_vecji (a, b, c) (x, y, z) =
  if a > x && b > y && c > z then true else false

(* c *)
let uporabi_ce_lahko f = function
  | None -> None
  | Some v -> Some (f v)

(* d *)

let pojavi_dvakrat element lst =
  let rec remove x = function
  | [] -> []
  | y :: ys -> if y == x then ys else y :: remove x ys
  in 
  if List.mem element (remove element (remove element lst)) then 
  false
  else if (List.mem element lst) && (List.mem element (remove element lst)) then
  true
  else false
    
(* e *)

let izracunaj_v_tocki a list =
  let rec izracunaj_v_tocki' acc = function
  | [] -> List.rev acc
  | f :: fs -> izracunaj_v_tocki' ((f a) :: acc) fs
  in izracunaj_v_tocki' [] list

(* f *)

let eksponent x p =
  let rec eksponent' acc = function
  | 0 -> acc
  | p -> eksponent' (x * acc) (p-1)
  in eksponent' 1 p

(*============================================================================*)

(* 2. NALOGA *)

(* a *)

type 'a mm_drevo =
  | Empty
  | Node of 'a mm_drevo * ('a * int) * 'a mm_drevo

let test = Node(Node(Empty, (1,3),Empty), (2,2), Node(Node(Empty,(4,1), Empty), (5,1), Node(Empty, (8,2), Empty)))

(* b *)

let rec vstavi tree element =
  match tree with
  | Empty -> Node(Empty, (element, 1), Empty)
  | Node(lt, (a, st_a), rt) ->
  if a = element then 
    Node(lt, (a, st_a + 1), rt)
  else if element < a then 
    Node(vstavi lt element, (a, st_a), rt)
  else
    Node(lt, (a, st_a), vstavi rt element)

(* c *)

let multimnozica_iz_seznama lst =
  let rec multimnozica_iz_seznama' acc = function
    | [] -> acc
    | x :: xs -> multimnozica_iz_seznama' (vstavi acc x) xs 
  in multimnozica_iz_seznama' Empty lst

(* d *)

let rec velikost_multimnozice = function
  | Empty -> 0
  | Node(lt, (a,st_a), rt) -> st_a + velikost_multimnozice lt + velikost_multimnozice rt

(* e *)

let seznam_iz_multimnozice tree =
  let rec seznam_iz_multimnozic' acc = function
    | Empty -> acc
    | Node(lt, (a,st_a), rt) -> 
    let rec pojavitev a acc = function
    | 0 -> acc
    | st_a -> pojavitev a (a :: acc) (st_a -1)
    in
   seznam_iz_multimnozic'(pojavitev a (seznam_iz_multimnozic' acc rt) st_a) lt
   in seznam_iz_multimnozic' [] tree
