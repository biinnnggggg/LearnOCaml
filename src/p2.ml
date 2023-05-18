(* Last Two Elements of a List *)
let rec last_two lst =
  match lst with
  | [] -> None
  | [a] -> None
  | [a;b] -> Some (a, b)
  | a::tl -> last_two tl;;

(* Testcases *)
let test1 = (last_two [] = None);;
let test2 = (last_two [1] = None);;
let test3 = (last_two [1 ; 2] = Some (1, 2));;
let test4 = (last_two [1 ; 2 ; 3] = Some (2, 3));;

(* Output
   
  val last_two : 'a list -> ('a * 'a) option = <fun>
  val test1 : bool = true
  val test2 : bool = true
  val test3 : bool = true
  val test4 : bool = true

*)