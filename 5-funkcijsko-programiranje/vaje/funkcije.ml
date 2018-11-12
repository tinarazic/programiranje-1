(* ========== Vaja 2: Funkcijsko Programiranje  ========== *)

(*----------------------------------------------------------------------------*]
Namig: Definirajte pomožno funkcijo za obračanje seznamov.
[*----------------------------------------------------------------------------*)

let rec reverse list =
  let rec reverse' list acc =
    match list, acc with
    | [], _ -> acc
    | x :: xs, _ -> reverse' xs (x :: acc)
  in reverse' list []

(*----------------------------------------------------------------------------*]
 Funkcija [repeat x n] vrne seznam [n] ponovitev vrednosti [x]. Za neprimerne
 vrednosti [n] funkcija vrne prazen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # repeat "A" 5;;
 - : string list = ["A"; "A"; "A"; "A"; "A"]
 # repeat "A" (-2);;
 - : string list = []
[*----------------------------------------------------------------------------*)

let rec repeat x n =
  if n <= 0 then
    []
  else 
    x :: repeat x (n-1)

(*----------------------------------------------------------------------------*]
 Funkcija [range] sprejme število in vrne seznam vseh celih števil od 0 do
 vključno danega števila. Za neprimerne argumente funkcija vrne prazen seznam.
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # range 10;;
 - : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
[*----------------------------------------------------------------------------*)

let rec range n =
  (*ni repno rekurzivna*)
  if n < 0 then
    []
  else
    (range (n-1)) @ [n]
    (* @ hoče sezname na obeh straneh zato daš n v ogalte oklepaje *)

let rec range n =
  let rec range' n acc = 
    if n < 0 then
      acc
    else 
      range' (n-1) (n :: acc)
  (* če na koncu dodajaš akumulatroju stvari se to izkaže za zelo počasno, kadar imaš dolge sezname -> glej testni primer*)
  in 
  range' n []

let rec test_bad n acc =
  if n < 0 then
    acc
  else 
    test_bad (n-1) (acc @ [0])

let rec test_good n acc =
  if n < 0 then
     acc
  else 
    test_good (n-1) (0 :: acc)
(*----------------------------------------------------------------------------*]
 Funkcija [map f list] sprejme seznam [list] oblike [x0; x1; x2; ...] in
 funkcijo [f] ter vrne seznam preslikanih vrednosti, torej
 [f(x0); f(x1); f(x2); ...].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (+)2 in
   map plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

let rec map f = function
(*ni repno rekurzivna*)
  | [] -> []
  | x :: xs -> f x :: map f xs

(*----------------------------------------------------------------------------*]
 Funkcija [map_tlrec] je repno rekurzivna različica funkcije [map].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (fun x -> x+2) in
   map_tlrec plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

let rec map_tlrec f list =
  let rec map_tlrec' f acc = function
    | [] -> acc
    | x :: xs -> map_tlrec' f ((f x) :: acc) xs
  in reverse (map_tlrec' f [] list)

(*----------------------------------------------------------------------------*]
 Funkcija [mapi] sprejme seznam in funkcijo dveh argumentov ter vrne seznam
 preslikanih vrednosti seznama, kjer kot drugi argument funkcije podamo indeks
 elementa v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mapi (+) [0; 0; 0; 2; 2; 2];;
 - : int list = [0; 1; 2; 5; 6; 7]
[*----------------------------------------------------------------------------*)

let rec mapi f list =
  let rec mapi' acc i f = function
    | [] -> acc
    | x :: xs -> mapi' ((f x i) :: acc) (i + 1) f xs
  in reverse (mapi' [] 0 f list)

(*----------------------------------------------------------------------------*]
 Funkcija [zip] sprejme dva seznama in vrne seznam parov istoležnih
 elementov podanih seznamov. Če seznama nista enake dolžine vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # zip [1; 1; 1; 1] [0; 1; 2; 3];;
 - : (int * int) list = [(1, 0); (1, 1); (1, 2); (1, 3)]
 # zip [1; 1; 1; 1] [1; 2; 3; 4; 5];;
 Exception: Failure "Different lengths of input lists.".
[*----------------------------------------------------------------------------*)

let rec zip list1 list2 = 
  let rec zip' acc list1 list2 = 
    match acc, list1, list2 with
    | acc, [], [] -> acc
    | acc, [], list2 -> failwith "Different lengths of input lists."
    | acc, list1, [] -> failwith "Different lengths of input lists."
    | acc, x :: xs, y:: ys -> zip' ((x, y) :: acc) xs ys
  in zip' [] list1 list2 |> reverse

(*----------------------------------------------------------------------------*]
 Funkcija [zip_enum_tlrec] sprejme seznama [x_0; x_1; ...] in [y_0; y_1; ...]
 ter vrne seznam [(0, x_0, y_0); (1, x_1, y_1); ...]. Funkcija je repno
 rekurzivna. Če seznama nista enake dolžine vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # zip_enum_tlrec ["a"; "b"; "c"] [7; 3; 4];;
 - : (int * string * int) list = [(0, "a", 7); (1, "b", 3); (2, "c", 4)]
[*----------------------------------------------------------------------------*)

let rec zip_enum_tlrec l1 l2 =
  let rec zip_enum_tlrec' acc i l1 l2 =
    match acc, i, l1, l2 with
    | acc, _, [], [] -> acc
    | acc, _, [], l2 -> failwith "Different lengths of input lists."
    | acc, _, l1, [] -> failwith "Different lengths of input lists."
    | acc, i, x :: xs, y :: ys -> zip_enum_tlrec' ((i, x, y) :: acc) (i + 1) xs ys
  in zip_enum_tlrec' [] 0 l1 l2 |> reverse

(*----------------------------------------------------------------------------*]
 Funkcija [unzip] je inverz funkcije [zip], torej sprejme seznam parov
 [(x0, y0); (x1, y1); ...] in vrne par seznamov ([x0; x1; ...], [y0; y1; ...]).
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let rec unzip = ()

(*----------------------------------------------------------------------------*]
 Funkcija [unzip_tlrec] je repno rekurzivna različica funkcije [unzip].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip_tlrec [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let rec unzip_tlrec = ()

(*----------------------------------------------------------------------------*]
 Funkcija [fold_left_no_acc f list] sprejme seznam [x0; x1; ...; xn] in
 funkcijo dveh argumentov [f] in vrne vrednost izračuna
 f(... (f (f x0 x1) x2) ... xn).
 V primeru seznama z manj kot dvema elementoma vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # fold_left_no_acc (^) ["F"; "I"; "C"; "U"; "S"];;
 - : string = "FICUS"
[*----------------------------------------------------------------------------*)

let rec fold_left_no_acc = ()

(*----------------------------------------------------------------------------*]
 Funkcija [apply_sequence f x n] vrne seznam zaporednih uporab funkcije [f] na
 vrednosti [x] do vključno [n]-te uporabe, torej
 [x; f x; f (f x); ...; (f uporabljena n-krat na x)].
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # apply_sequence (fun x -> x * x) 2 5;;
 - : int list = [2; 4; 16; 256; 65536; 4294967296]
 # apply_sequence (fun x -> x * x) 2 (-5);;
 - : int list = []
[*----------------------------------------------------------------------------*)

let rec apply_sequence = ()

(*----------------------------------------------------------------------------*]
 Funkcija [filter f list] vrne seznam elementov [list], pri katerih funkcija [f]
 vrne vrednost [true].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # filter ((<)3) [0; 1; 2; 3; 4; 5];;
 - : int list = [4; 5]
[*----------------------------------------------------------------------------*)

let rec filter = ()

(*----------------------------------------------------------------------------*]
 Funkcija [exists] sprejme seznam in funkcijo, ter vrne vrednost [true] čim
 obstaja element seznama, za katerega funkcija vrne [true] in [false] sicer.
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # exists ((<)3) [0; 1; 2; 3; 4; 5];;
 - : bool = true
 # exists ((<)8) [0; 1; 2; 3; 4; 5];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec exists = ()

(*----------------------------------------------------------------------------*]
 Funkcija [first f default list] vrne prvi element seznama, za katerega
 funkcija [f] vrne [true]. Če takšnega elementa ni, vrne [default].
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # first ((<)3) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 5
 # first ((<)8) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 0
[*----------------------------------------------------------------------------*)

let rec first = ()
