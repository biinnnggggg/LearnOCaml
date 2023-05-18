(* Length of a List
   without tail recursion *)
let rec length lst =
  match lst with
  | [] -> 0
  | a::tl -> 1 + length tl;;

(* Testcases *)
let test1 = (length [] = 0);;
let test2 = (length [1] = 1);;
let test3 = (length [1 ; 2] = 2);;

(* Output

  val length : 'a list -> int = <fun>
  val test1 : bool = true
  val test2 : bool = true
  val test3 : bool = true

*)

(* Length of a List
   with tail recursion *)
let length_tailrec lst =
  let rec helper lst acc = 
    match lst with
    | [] -> acc
    | a::tl -> helper tl (acc + 1) 
  in
    helper lst 0;;
  
(* Testcases *)
let test4 = (length_tailrec [] = 0);;
let test5 = (length_tailrec [1] = 1);;
let test6 = (length_tailrec [1 ; 2] = 2);;
  
(* Output
  
  val length_tailrec : 'a list -> int = <fun>
  val test4 : bool = true
  val test5 : bool = true
  val test6 : bool = true
  
*)