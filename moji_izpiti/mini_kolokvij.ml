(* -------- 1 -------- *)
let rec int_sum int_list =
  let rec int_sum' acc = function
    | [] -> acc
    | x :: xs -> int_sum' (x + acc) xs
  in int_sum' 0 int_list 
(* -------- 2 -------- *)
let rec narasca = function
  | [] | _ :: [] -> true
  | x :: y :: xs -> if x < y && narasca xs then true else false
(* -------- 3 -------- *)
let rec vstavi x = function
  | [] -> [x]
  | y :: [] -> if x < y then x :: y :: [] else y :: x :: []
  | y :: ys -> if x > y then y :: vstavi x ys else x :: y :: ys

let rec uredi int_list =
  let rec uredi' acc = function
    | [] -> acc
    | x :: xs -> uredi' (vstavi x acc) xs
  in uredi' [] int_list
(* -------- 4 -------- *)
let rec splosni_uredi cmp list =
  let rec splosni_vstavi x = function
    | [] -> [x]
    | y :: [] -> if (cmp x y) then x :: y :: [] else y :: x :: []
    | y :: ys -> if (cmp y x) then y :: splosni_vstavi x ys else x :: y :: ys
  in 
  let rec splosni_uredi' acc = function
  | [] -> acc
  | x :: xs -> splosni_uredi' (splosni_vstavi x acc) xs
  in splosni_uredi' [] list
(* -------- 5 -------- *)

type priority =
  | Top
  | Group of int

type status =
  | Staff
  | Passenger of priority

type flyer = { status : status ; name : string }

let flyers = [ {status = Staff; name = "Quinn"}
             ; {status = Passenger (Group 0); name = "Xiao"}
             ; {status = Passenger Top; name = "Jaina"}
             ; {status = Passenger (Group 1000); name = "Aleks"}
             ; {status = Passenger (Group 1000); name = "Robin"}
             ; {status = Staff; name = "Alan"}
             ]

             
(* -------- 6 -------- *)
(* ZMANJKALO ČASA :( *)
(*IDEJA:
let vkrcavanje potniki =
  let stopnja =
  match status with
    | Staff -> 1
    | Passenger x ->(
      match x with
      | Top -> 2
      | Group y -> y + 2
    )
  in splosni_uredi (fun j k -> j < k) potniki*)

  
(* -------- 7 -------- *)