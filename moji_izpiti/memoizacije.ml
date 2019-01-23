(* PYTHON *)

(* Naša funckija *)
(*

def memoiziraj(f):
    rezultati = {}
    def mem_f(x):
        if x not in rezultati:
            rezultati[x] = f(x)
        return rezultati[x]
    return mem_f
    
@memoiziraj 

*)

(* Vgrajena funckija *)
(*

from functools import lru_cache
@lru_cache(maxsize=None)

*)


(* OCAML *)

(* Memoizacija poljubne funkcije *)

let memoiziraj f =
  let rezultati = Hashtbl.create 512 in
  let mem_f x =
    match Hashtbl.find_opt rezultati x with
    | None ->
        let y = f x in
        Hashtbl.add rezultati x y;
        y
    | Some y ->
        y
  in
  mem_f

(* Memoizacija rekurziven funkcije *)

let memoiziraj_rec odviti_f =
  let rezultati = Hashtbl.create 512 in
  let rec mem_f x =
    match Hashtbl.find_opt rezultati x with
    | None ->
        let y = odviti_f mem_f x in
        Hashtbl.add rezultati x y;
        y
    | Some y ->
        y
  in
  mem_f

(* Memoizacija dveh rekurzivnih funckij, ki kličeta ena drugo *)

let memoiziraj_rec2 odviti_f odviti_g =
  let rezultati_f = Hashtbl.create 512 in
  let rezultati_g = Hashtbl.create 512 in
  let rec mem_f x =
    match Hashtbl.find_opt rezultati_f x with
    | None ->
        let y = odviti_f mem_f mem_g x in
        Hashtbl.add rezultati_f x y;
        y
    | Some y ->
        y
  and mem_g x =
    match Hashtbl.find_opt rezultati_g x with
    | None ->
        let y = odviti_g mem_f mem_g x in
        Hashtbl.add rezultati_g x y;
        y
    | Some y ->
        y
  in
  mem_f, mem_g