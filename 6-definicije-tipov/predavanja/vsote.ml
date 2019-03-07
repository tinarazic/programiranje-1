type geometrijski_objekt =
  | Tocka
  | Krog of float
  | Pravokotnik of float * float

let povrsina = function
  | Tocka -> 0.
  | Krog r -> 3.14 *. r ** 2.
  | Pravokotnik (v, s) -> v *. s

type cevlji = Cevelj of float
type metri = Meter of float

let cevlji_v_metre (Cevelj c) = Meter (0.3048 *. c)
let metri_v_cevlje (Meter m) = Cevelj (m /. 0.3048)

type ('a, 'b) vsota = 
    | Iota1 of 'a
    | Iota2 of 'b

(* (A + B) x C = A x C + B x C *)
(* ('a, 'b) vsota * 'c -> ('a * 'c, 'b * 'c) vsota *)
let fi = function
    | (Iota1 a, c) -> Iota1 (a, c)
    | (Iota2 b, c) -> Iota2 (b, c)

(* A x B + C x B = (A + C) x B *)
(* zamenjam črke, ker OCaml tako deluje *)
let inverz_fi nekaj_iz_vsote = 
  match nekaj_iz_vsote with
  | Iota1 (a, c) -> (Iota1 a, c)
  | Iota2 (b, c) -> (Iota2 b, c)

(* C^(A + B) -> C^A x C^B  *) (* ?? *)