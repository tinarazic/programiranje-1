
(* ========== Vaja 1: Uvod v OCaml  ========== *)

(*----------------------------------------------------------------------------*]
 Funkcija [penultimate_element] vrne predzadnji element danega seznama. V
 primeru prekratkega seznama vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # penultimate_element [1; 2; 3; 4];;
 - : int = 3
[*----------------------------------------------------------------------------*)

let rec ultimate_element list =
  match list with
  | [] -> failwith "List too short."
  | y :: [] -> y
  | y :: ys ->  ultimate_element(ys)


let rec penultimate_element list =
  match list with
  | [] -> failwith "List too short."
  | x :: [] -> failwith "List too short."
  | x :: y :: [] -> x
  | x :: y :: ys ->  penultimate_element(y :: ys)

(* polepšamo funkcijo*)
(* glej zapiske *)
let rec penultimate_element = function
  | [] | _ :: [] -> failwith "List too short."
  | x :: y :: [] -> x
  | _ :: y :: ys ->  penultimate_element(y :: ys)

let rec penultimate_element = function
  | [] | [_] -> failwith "List too short!"
  | x :: _ :: []-> x
  | _ :: xs -> penultimate_element xs
(*----------------------------------------------------------------------------*]
 Funkcija [get k list] poišče [k]-ti element v seznamu [list]. Številčenje
 elementov seznama (kot ponavadi) pričnemo z 0. Če je k negativen, funkcija
 vrne ničti element. V primeru prekratkega seznama funkcija vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # get 2 [0; 0; 1; 0; 0; 0];;
 - : int = 1
[*----------------------------------------------------------------------------*)

let rec get k = function
  | [] -> failwith "List is too short."
  | x :: xs -> if k <= 0 then x else get (k-1) xs


let rec get k = function
(* po navodilih, poelpšana verzija: k ne uporabljamo, zato lahko kar function uporabimo*)
  | [] -> failwith "List too short!"
  | x :: xs when k <= 0 -> x
  | x :: xs -> get (k - 1) xs

(*----------------------------------------------------------------------------*]
 Funkcija [double] podvoji pojavitve elementov v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # double [1; 2; 3];;
 - : int list = [1; 1; 2; 2; 3; 3]
[*----------------------------------------------------------------------------*)

let rec double = function
 | [] -> []
 | x :: [] -> [x; x]
 | x :: xs -> x :: x :: double xs

let rec double = function
 | [] -> []
 | x :: xs -> x :: x :: double xs

(*----------------------------------------------------------------------------*]
 Funkcija [divide k list] seznam razdeli na dva seznama. Prvi vsebuje prvih [k]
 elementov, drugi pa vse ostale. Funkcija vrne par teh seznamov. V primeru, ko
 je [k] izven mej seznama, je primeren od seznamov prazen.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # divide 2 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2], [3; 4; 5])
 # divide 7 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2; 3; 4; 5], [])
[*----------------------------------------------------------------------------*)

let rec divide k list =
  match k, list with 
  | k, list  when (k <= 0) -> ([],list)
  | k, [] -> ([],[])
  | k, x :: xs ->
   let (left_list, right_list) = divide (k - 1) xs in
   (x :: left_list, right_list)

(*----------------------------------------------------------------------------*]
 Funkcija [delete k list] iz seznama izbriše [k]-ti element. V primeru
 prekratkega seznama funkcija vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # delete 3 [0; 0; 0; 1; 0; 0];;
 - : int list = [0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)
let delete k list = 
  match k, list with
  | _, [] -> failwith "List too short!"
  | k, list -> let (f, s) = divide k list 
  in (
    match s with
    | [] -> f
    | z :: zs -> f @ zs 
  )

(* LAŽJE!!!!*)
let rec delete k = function
  | [] -> failwith "List is too short."
  | x :: xs -> if k = 0 then xs else x :: delete (k-1) xs


let rec delete k  = function
  | [] -> failwith "List too short!"
  | x :: xs when k = 0 -> xs
  | x :: xs -> x :: delete (k - 1) xs

(*----------------------------------------------------------------------------*]
 Funkcija [slice i k list] sestavi nov seznam, ki vsebuje elemente seznama
 [list] od vključno [i]-tega do izključno [k]-tega. Predpostavimo, da sta [i] in
 [k] primerna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # slice 3 6 [0; 0; 0; 1; 2; 3; 0; 0];;
 - : int list = [1; 2; 3]
[*----------------------------------------------------------------------------*)

let rec slice i k list =
  let (l1,l2) = divide i list in 
  let (s1,s2) = divide (k-i) l2 in
  s1

(*----------------------------------------------------------------------------*]
 Funkcija [insert x k list] na [k]-to mesto seznama [list] vrine element [x].
 Če je [k] izven mej seznama, ga funkcija doda na začetek oziroma na konec.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 1 3 [0; 0; 0; 0; 0];;
 - : int list = [0; 0; 0; 1; 0; 0]
 # insert 1 (-2) [0; 0; 0; 0; 0];;
 - : int list = [1; 0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec insert x k list =
  let (l1,l2) = divide k list in
  let rep = x :: l2 in
  l1 @ rep

let rec insert x k = function
  | [] -> [x]
  | y :: ys -> if k <= 0 then x :: y :: ys else y :: insert x k ys

(*----------------------------------------------------------------------------*]
 Funkcija [rotate n list] seznam zavrti za [n] mest v levo. Predpostavimo, da
 je [n] v mejah seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # rotate 2 [1; 2; 3; 4; 5];;
 - : int list = [3; 4; 5; 1; 2]
[*----------------------------------------------------------------------------*)

let rec rotate n list =
  let (l1,l2) = divide n list in 
  l2 @ l1

(*----------------------------------------------------------------------------*]
 Funkcija [remove x list] iz seznama izbriše vse pojavitve elementa [x].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # remove 1 [1; 1; 2; 3; 1; 2; 3; 1; 1];;
 - : int list = [2; 3; 2; 3]
[*----------------------------------------------------------------------------*)

let rec remove x = function
  | [] -> []
  | y :: []  when y = x -> []
  | y :: ys when y = x-> remove x ys
  | y :: ys -> y :: remove x ys

let rec remove x = function
  | [] -> []
  | y :: ys -> if y == x then remove x ys else y :: remove x ys

(*----------------------------------------------------------------------------*]
 Funkcija [is_palindrome] za dani seznam ugotovi ali predstavlja palindrom.
 Namig: Pomagaj si s pomožno funkcijo, ki obrne vrstni red elementov seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_palindrome [1; 2; 3; 2; 1];;
 - : bool = true
 # is_palindrome [0; 0; 1; 0];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec is_palindrome list =
  let rec reverse = function
    | [] -> []
    | x :: xs -> reverse xs @ [x] in
  list = reverse list   
  
(*----------------------------------------------------------------------------*]
 Funkcija [max_on_components] sprejme dva seznama in vrne nov seznam, katerega
 elementi so večji od istoležnih elementov na danih seznamih. Skupni seznam ima
 dolžino krajšega od danih seznamov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_on_components [5; 4; 3; 2; 1] [0; 1; 2; 3; 4; 5; 6];;
 - : int list = [5; 4; 3; 3; 4]
[*----------------------------------------------------------------------------*)

let rec max_on_components l1 l2 =
  match l1, l2 with
  | [], l2  -> []
  | l1, [] -> []
  | x :: xs, y :: ys -> max x y :: max_on_components xs ys

(*----------------------------------------------------------------------------*]
 Funkcija [second_largest] vrne drugo največjo vrednost v seznamu. Pri tem se
 ponovitve elementa štejejo kot ena vrednost. Predpostavimo, da ima seznam vsaj
 dve različni vrednosti.
 Namig: Pomagaj si s pomožno funkcijo, ki poišče največjo vrednost v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # second_largest [1; 10; 11; 11; 5; 4; 10];;
 - : int = 10
[*----------------------------------------------------------------------------*)

let rec second_largest list =
  let rec max_list = function
    | [] -> failwith "List too short!"
    | x :: [] -> x
    | x :: ys -> (max x (max_list ys)) 
  in max_list (remove (max_list list) list)
