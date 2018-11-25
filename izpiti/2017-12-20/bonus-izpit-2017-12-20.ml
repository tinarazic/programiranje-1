(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

(* 1.1) Definirajte funkcijo, ki vzame par in zamenja komponenti para.
   Primer: /obrni (2, 4) = (4, 2)/ *)
 let obrni (x, y) = (y, x)

(* 1.2) Definirajte funkcijo, ki vzame par p in vrednost x in zamenja drugo
   komponento para p z x.
   Primer: /zamenjaj_drugo (2, 4) 7 = (2, 7)/ *)
 let zamenjaj_drugo (p1, p2) x = (p1, x)

(* 1.3) Definirajte funkcijo, ki vzame seznam parov in izračuna nov seznam parov,
   ki imajo drugo komponento zamenjano z 42.
   Primer: /vsem_zamenjaj_drugo_z_42 [(12, 1); (2, 4)] = [(12, 42); (2, 42)]/ *)
 let rec vsem_zamenjaj_drugo_z_42 = function
  | [] -> []
  | (x, y) :: xs -> (x, 42) :: vsem_zamenjaj_drugo_z_42 xs

(* 1.4) Definirajte funkcijo, ki varno vrne glavo seznama v primeru, ko seznam ni prazen.
   Uporabite tip option.
   Primer: /glava [1; 2; 3] = Some 1/ *)
 let glava = function
  | [] -> None
  | x :: xs -> Some x

(* 1.5) Definirajte funkcijo, vzame funkcijo (f: 'a -> 'b), neko vrednost (x : 'a) in
   celo število n. Funkcija naj vrne vrednost, ki jo dobimo če f n-krat uporabimo na x,
   torej f (f ... (f x)...).
   Primer: /uporabi_veckrat succ 0 420 = 420/ *)
 let rec uporabi_veckrat f x n =
  match n with
    | 0 -> x
    | _ -> uporabi_veckrat f (f x) (n - 1)

(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

(* 2.1) Rožno drevo je drevo, v katerem ima vsak koren poljubno mnogo otrok,
   ki so zopet rožna drevesa. Rožna drevesa predstavimo s parametričnim
   tipom /'a drevo/ z enim konstruktorjem, ki sprejme:
   - vrednost (koren) tipa /'a/ in
   - seznam (gozd) dreves tipa /'a drevo/. *)
type 'a drevo = T of 'a * 'a drevo list

(* 2.2) Definirajte naslednja rožna drevesa:

   t = 1,  t' =  2   ,      t'' =  3
                / \               /| \
               t  t              / |  \
                               -1  t'  0

 *)

let t = T(1, [])
let t' = T(2, [t; t])
let t'' = T(3, [T(-1,[]); t'; T(0,[])])

(* 2.3) Definirajte funkcijo, ki vrne gozd rožnega drevesa. *)
let vrni_gozd (T(_, gozd)) = gozd

(* 2.4) Definirajte funkcijo, ki izpiše vse vrednosti v rožnem drevesu celih števil.
   Števila naj bodo v ločenih vrsticah. Uporabite (print_int : int -> unit) in
   (print_newline : unit -> unit). *)

  let izpisi_vrednosti = function
   | T (k, []) -> print_newline (print_int k)
   | T (k, xs) -> 
   let rec aux acc = function
   | [] -> print_newline (print_int acc)
   | T (k, []) :: [] -> print_newline (print_int acc); aux k []
   | T (k, x) :: xs -> print_newline (print_int acc); aux k (x @ xs)
   in
   aux k xs


(* 2.5) Definirajte funkcijo, ki izračuna globino rožnega drevesa, t.j. dolžino
   najdaljše poti od korena do lista. *)
   let rec max_number_list = function
    | [] -> 0
    | x::[] -> x
    | x::xs -> max x (max_number_list xs)
 
  let rec globina = function
    | T(k, []) -> 1
    | T(k, g) -> 1 + max_number_list (List.map globina g)

(* 2.6) Definirajte funkcijo, ki sestavi (poljubno) rožno drevo globine n.
   Vrednosti v korenih so poljubne. *)

let rec globoko_drevo = function
	| 1 -> T(Random.int 5, []) (* Vsako drevo je globine vsaj 1*)
  | n -> T(Random.int 5, [globoko_drevo (n - 1)])
  
let globoko_drevo2 n =
  let rec aux acc n =
    if n > 0
    then aux (T (Random.int 5, [acc])) (n-1)
    else acc
  in aux (T (Random.int 5, [])) (n-1)

(* 2.7) Definirajte funkcijo, ki sprejme funkcijo (f : 'b -> 'a -> 'b) in začetno vrednost (acc : 'b)
   in funkcijo f zloži [fold] preko drevesa (t : 'a drevo). Vrstni red pri tem ni pomemben.

   Za primer t' želimo vrednost f (f (f acc 1) 2) 2)  (lahko tudi f (f (f acc 2) 1) 2))
   Primer: /zlozi (fun acc x -> x + acc) 0 t'' = 6/

   Če želite vse točke, mora biti funkcija repno rekurzivna.

   Opomba: kot ste videli na vajah, nekatere funkcije iz modula List,
   na primer List.map, niso repno rekurzivne, zato se jim raje
   izognite. *)
   
   let rec zlozi f acc = function
    | T (k, xs) -> 
    let rec aux acc l = function
      | [] -> f acc l
      | T (k, x) :: xs -> aux (f acc l) (k) (x @ xs)
    in
    aux acc k xs
