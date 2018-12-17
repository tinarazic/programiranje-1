(* ========== Vaja 4: Iskalna Drevesa  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Ocaml omogoča enostavno delo z drevesi. Konstruiramo nov tip dreves, ki so
 bodisi prazna, bodisi pa vsebujejo podatek in imajo dve (morda prazni)
 poddrevesi. Na tej točki ne predpostavljamo ničesar drugega o obliki dreves.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)
type 'a tree =
     | Empty
     | Node of 'a tree * 'a * 'a tree


(* loči liste od ostalih vozlišč
type 'a drevo =
     | Empty
     | Leaf 'a
     | Node of 'a drevo * 'a * 'a drevo

da imamo liste not je kozmetično
     Leaf x = Node(Empty,x,Empty)
*)

let leaf x = Node (Empty, x, Empty)

(*----------------------------------------------------------------------------*]
 Definirajmo si testni primer za preizkušanje funkcij v nadaljevanju. Testni
 primer predstavlja spodaj narisano drevo, pomagamo pa si s pomožno funkcijo
 [leaf], ki iz podatka zgradi list.
          5
         / \
        2   7
       /   / \
      0   6   11
[*----------------------------------------------------------------------------*)

let test_tree =
     let left_t = Node(leaf 0, 2, Empty) in
     let right_t = Node(leaf 6, 7, leaf 11) in
     Node(left_t, 5, right_t)

(*----------------------------------------------------------------------------*]
 Funkcija [mirror] vrne prezrcaljeno drevo. Na primeru [test_tree] torej vrne
          5
         / \
        7   2
       / \   \
      11  6   0
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mirror test_tree ;;
 - : int tree =
 Node (Node (Node (Empty, 11, Empty), 7, Node (Empty, 6, Empty)), 5,
 Node (Empty, 2, Node (Empty, 0, Empty)))
[*----------------------------------------------------------------------------*)

let rec mirror = function
     | Empty -> Empty
     | Node(lt, x, rt) -> Node(mirror rt, x, mirror lt)

(*vsako od podreves mormo še rekurzivno prezrcalit..*)
(*----------------------------------------------------------------------------*]
 Funkcija [height] vrne višino oz. globino drevesa, funkcija [size] pa število
 vseh vozlišč drevesa.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # height test_tree;;
 - : int = 3
 # size test_tree;;
 - : int = 6
[*----------------------------------------------------------------------------*)

let rec height = function
     | Empty -> 0
     | Node (lt, x, rt) -> 1 + max (height lt) (height rt)


let rec size = function
     | Empty -> 0
     | Node (lt, x, rt) -> 1 + size lt + size rt

(* REPNO REKURZIVNI SIZE- izpit! *)
let tl_rec_size tree =
     let rec size' acc queue =
          (*Pogledamo, kateri je naslednji element v VRSTI za obravnavo. *)
          match queue with
          | [] -> acc
          | t :: ts -> (
               (*Obravnavamo drevo.*)
               match t with
               | Empty -> size' acc ts (*Prazno drevo smao odstranimo iz vrste*)
               | Node (lt, x, rt) -> 
               let new_acc = acc + 1 in (*Obravnavamo vozlišče.*)
               let new_queue = lt :: rt :: ts in (*Dodamo pdodrevesa v vrsto.*)
               size' new_acc new_queue
          )
     in
     (*Zaženemo pomožno funckijo.*)
     size' 0 [tree]


(*----------------------------------------------------------------------------*]
 Funkcija [map_tree f tree] preslika drevo v novo drevo, ki vsebuje podatke
 drevesa [tree] preslikane s funkcijo [f].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # map_tree ((<)3) test_tree;;
 - : bool tree =
 Node (Node (Node (Empty, false, Empty), false, Empty), true,
 Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
[*----------------------------------------------------------------------------*)

let rec map_tree f = function
     | Empty -> Empty
     | Node (lt, x, rt) -> Node(map_tree f lt, f x, map_tree f rt)

(*----------------------------------------------------------------------------*]
 Funkcija [list_of_tree] pretvori drevo v seznam. Vrstni red podatkov v seznamu
 naj bo takšen, da v primeru binarnega iskalnega drevesa vrne urejen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # list_of_tree test_tree;;
 - : int list = [0; 2; 5; 6; 7; 11]
[*----------------------------------------------------------------------------*)

let rec list_of_tree = function
     | Empty -> []
     | Node(lt, x, rt) -> (list_of_tree lt) @  x :: (list_of_tree rt)
(*----------------------------------------------------------------------------*]
 Funkcija [is_bst] preveri ali je drevo binarno iskalno drevo (Binary Search 
 Tree, na kratko BST). Predpostavite, da v drevesu ni ponovitev elementov, 
 torej drevo npr. ni oblike Node( leaf 1, 1, leaf 2)). Prazno drevo je BST.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_bst test_tree;;
 - : bool = true
 # test_tree |> mirror |> is_bst;;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec is_sorted = function
     | [] -> true
     | x :: [] -> true
     | x :: y :: xs -> if x < y then is_sorted (y :: xs) else false

let is_bst tree =
     let lst = list_of_tree tree in
     if is_sorted lst then true else false


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 V nadaljevanju predpostavljamo, da imajo dvojiška drevesa strukturo BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [insert] v iskalno drevo pravilno vstavi dani element. Funkcija 
 [member] preveri ali je dani element v iskalnem drevesu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 2 (leaf 4);;
 - : int tree = Node (Node (Empty, 2, Empty), 4, Empty)
 # member 3 test_tree;;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec insert x = function
     | Empty -> leaf x
     | Node(lt, root, rt) -> 
     if x < root then 
     Node(insert x lt, root, rt)
     else
     Node(lt, x, insert x rt)

let rec member x = function
     | Empty -> false
     | Node (lt, root, rt) -> 
     if x = root then
     true
     else if x < root then
     member x lt
     else
     member x rt

(*----------------------------------------------------------------------------*]
 Funkcija [member2] ne privzame, da je drevo bst.
 
 Opomba: Premislte kolikšna je časovna zahtevnost funkcije [member] in kolikšna
 funkcije [member2] na drevesu z n vozlišči, ki ima globino log(n). 
[*----------------------------------------------------------------------------*)

let rec member2 x = function
     | Empty -> false
     | Node(lt, root, rt) ->
     if x = root then
     true
     else if member2 x lt = false then
     member2 x rt
     else
     true

(*----------------------------------------------------------------------------*]
 Funkcija [succ] vrne naslednjika korena danega drevesa, če obstaja. Za drevo
 oblike [bst = Node(l, x, r)] vrne najmanjši element drevesa [bst], ki je večji
 od korena [x].
 Funkcija [pred] simetrično vrne največji element drevesa, ki je manjši od
 korena, če obstaja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # succ test_tree;;
 - : int option = Some 6
 # pred (Node(Empty, 5, leaf 7));;
 - : int option = None
[*----------------------------------------------------------------------------*)

let rec min = function
    | Empty -> None
    | Node (Empty, x, _) -> Some x
    | Node(lt, x, rt) -> min lt 

let rec succ = function
    | Empty -> None
    | Node(lt, x, rt) -> min rt



let rec max = function
    | Empty -> None
    | Node (_, x, Empty) -> Some x
    | Node(lt, x, rt) -> max rt 


let rec pred = function
    | Empty -> None
    | Node(lt, x, rt) -> max lt


(*----------------------------------------------------------------------------*]
 Na predavanjih ste omenili dva načina brisanja elementov iz drevesa. Prvi 
 uporablja [succ], drugi pa [pred]. Funkcija [delete x bst] iz drevesa [bst] 
 izbriše element [x], če ta v drevesu obstaja. Za vajo lahko implementirate
 oba načina brisanja elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # (*<< Za [delete] definiran s funkcijo [succ]. >>*)
 # delete 7 test_tree;;
 - : int tree =
 Node (Node (Node (Empty, 0, Empty), 2, Empty), 5,
 Node (Node (Empty, 6, Empty), 11, Empty))
[*----------------------------------------------------------------------------*)

let rec delete x tree =
     match tree with
     | Empty -> Empty (*Empty case*)
     | Node(Empty, y, Empty) when x = y -> Empty (*Leaf case*)
     | Node(Empty, y, rt) when x = y -> rt (*One sided*)
     | Node(lt, y, Empty) when x = y -> lt (*One sided*)
     | Node(lt, y, rt) when x <> y -> (*Recurse deeper*)
     if  y < x then
          Node(lt, y, delete x rt)
     else 
          Node(delete x lt, y, rt)
     | Node(lt, y, rt) -> (*SUPER FUN CASE*)
          match succ tree with
          | None -> failwith "How is this possible?!" (*this cannot happen :D*) (*Case ki bi ga dobil smo že pokril*)
          | Some z -> Node(lt, z, delete z rt)
  

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 SLOVARJI

 S pomočjo BST lahko (zadovoljivo) učinkovito definiramo slovarje. V praksi se
 slovarje definira s pomočjo hash tabel, ki so še učinkovitejše. V nadaljevanju
 pa predpostavimo, da so naši slovarji [dict] binarna iskalna drevesa, ki v
 vsakem vozlišču hranijo tako ključ kot tudi pripadajočo vrednost, in imajo BST
 strukturo glede na ključe. Ker slovar potrebuje parameter za tip ključa in tip
 vrednosti, ga parametriziramo kot [('key, 'value) dict].
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type ('k, 'v) dict =
     | Empty 
     | Node of ('k, 'v) dict * ('k * 'v) * ('k, 'v) dict
     
let leaf (k, v) = Node(Empty, (k, v), Empty)

(*----------------------------------------------------------------------------*]
 Napišite testni primer [test_dict]:
      "b":1
      /    \
  "a":0  "d":2
         /
     "c":-2
[*----------------------------------------------------------------------------*)

let test_dict = 
     let left = leaf ("a", 0) in
     let right = Node(leaf ("c",-2), ("d",2), Empty) in 
     Node(left, ("b",1), right)

(*----------------------------------------------------------------------------*]
 Funkcija [dict_get key dict] v slovarju poišče vrednost z ključem [key]. Ker
 slovar vrednosti morda ne vsebuje, vrne [option] tip.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_get "banana" test_dict;;
 - : 'a option = None
 # dict_get "c" test_dict;;
 - : int option = Some (-2)
[*----------------------------------------------------------------------------*)

let rec dict_get key = function
     | Empty -> None
     | Node (ld, (k, v), rd) ->
     if key = k then 
     Some v 
     else if key < k then
     dict_get key ld
     else
     dict_get key rd
      
(*----------------------------------------------------------------------------*]
 Funkcija [print_dict] sprejme slovar s ključi tipa [string] in vrednostmi tipa
 [int] in v pravilnem vrstnem redu izpiše vrstice "ključ : vrednost" za vsa
 vozlišča slovarja.
 Namig: Uporabite funkciji [print_string] in [print_int]. Nize združujemo z
 operatorjem [^]. V tipu funkcije si oglejte, kako uporaba teh funkcij določi
 parametra za tip ključev in vrednosti v primerjavi s tipom [dict_get].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # print_dict test_dict;;
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let rec print_dict

(*----------------------------------------------------------------------------*]
 Funkcija [dict_insert key value dict] v slovar [dict] pod ključ [key] vstavi
 vrednost [value]. Če za nek ključ vrednost že obstaja, jo zamenja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_insert "1" 14 test_dict |> print_dict;;
 1 : 14
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
 # dict_insert "c" 14 test_dict |> print_dict;;
 a : 0
 b : 1
 c : 14
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

