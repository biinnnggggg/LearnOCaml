(* Tail of a List *)
let rec last alist =
  match alist with
  | [] -> None
  | [a] -> Some a
  | a::tl -> last tl;;

(* Testcases *)
let test1 = (last [] = None);;
let test2 = (last ["a"] = Some "a");;
let test3 = (last ["a" ; "b" ; "c"] = Some "c");;

(* Output

  val last : 'a list -> 'a option = <fun>
  val test1 : bool = true
  val test2 : bool = true
  val test3 : bool = true

*)
