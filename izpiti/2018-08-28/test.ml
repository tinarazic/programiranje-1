(* ==== PRVA NALOGA ==== *)
(* a *)
let razlika_kvadratov i j =
  (i + j) * (i + j) - (i * i + j * j)

(* b *) 
let uporabi_na_paru f (x, y) =
  (f x, f y)

(* c *)
let rec ponovi_seznam n sez =
  if n <= 0 then
    []
  else
    sez @ (ponovi_seznam (n-1) sez)

(* d *)
let razdeli sez =
  let rec razdeli' n_acc p_acc = function
    | [] -> (List.rev n_acc, List.rev p_acc)
    | x :: xs when x < 0 -> razdeli' (x :: n_acc) p_acc xs
    | x :: xs -> razdeli' n_acc (x :: p_acc) xs
  in razdeli' [] [] sez

(*===== DRUGA NALOGA ===== *)

type 'a tree = 
  | Empty 
  | Node of 'a tree * 'a * 'a tree

let leaf x = Node( Empty, x, Empty)

let test1 = 
  Node(
    Node(leaf 3, 10, Node(leaf 14, 13, leaf 6)),
    11,
    Node( leaf 2, 8, leaf 10)
  )

let rec padajoca v = function
  | Empty -> []
  | Node (lt, x , rt) when x > v -> [] 
  | Node (lt, x, rt) ->
     let left = padajoca x lt in 
     let right = padajoca x rt in 
     if List.length left > List.length right then
      left @ [x]
    else
      right @ [x]

let rec narascajoca v = function
  | Empty -> []
  | Node (lt, x , rt) when x < v -> [] 
  | Node (lt, x, rt) ->
     let left = narascajoca x lt in 
     let right = narascajoca x rt in 
     if List.length left > List.length right then
      x :: left
    else
      x :: right

let rec monotona_pot = function
  | Empty -> []
  | Node (lt, x, rt) ->
  (*Recursive search for paths *)
    let pure_left = monotona_pot lt in 
    let pure_right = monotona_pot rt in 
    let left_to_right = (padajoca x lt) @ [x] @ (narascajoca x rt) in 
    let right_to_left = (padajoca x rt) @ [x] @ (narascajoca x lt) in
    (*Choose the longest one*)
    let options = [pure_right; left_to_right; right_to_left]  in 
    let pick_bigger x y = if List.length x > List.length y then x else y in
    List.fold_left pick_bigger pure_left options

(*TRETJA NALOGA*)

type 'a veriga = 
  | Filter of ('a -> bool) * 'a list * 'a veriga
  | Ostalo of 'a list

(* a *)
let test = 
  Filter((fun x -> x < 0), [],
  Filter ((fun x -> x < 10), [],
  Ostalo []))

(* b *)
let rec vstavi x veriga =
  match veriga with 
  | Ostalo (elementi) -> Ostalo (x :: elementi)
  | Filter (f, elementi, filtri) ->
    if f x then 
      Filter(f, x :: elementi, filtri)
    else  
      Filter(f, elementi, vstavi x filtri)

(* c *)
let rec poisci x = function
  | Ostalo elementi -> List.mem x elementi
  | Filter (f, elementi, filtri) ->
    if f x then 
    List.mem x elementi
    else 
    poisci x filtri

(* d *)
let rec izprazni_filtre = function
  | Ostalo elementi -> (Ostalo [], elementi)
  | Filter (f, elementi, filtri) ->
    let prazni_filtri, pobrani_elementi = izprazni_filtre filtri in
    let vsi_elementi = elementi @ pobrani_elementi in 
    (Filter (f, [], prazni_filtri), vsi_elementi)


(* e *)
let dodaj f veriga =
  let veriga' = Filter(f, [], veriga) in 
  let prazna_veriga, elementi = izprazni_filtre veriga' in 
  List.fold_left (fun v x -> vstavi x v) prazna_veriga elementi

