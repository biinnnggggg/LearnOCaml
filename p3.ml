(* N-th Element of a List *)

let rec nth_element alist n =
  match alist with
  | [] -> raise (Failure "nth")
  | [a] -> if n = 0 then Some a else raise (Failure "nth")
  | a::tl -> if n = 0 then Some a else nth_element tl (n - 1);;

(* Testcases *)

let test1 = (nth_element [] 0);; (* Exception: Failure "nth" *)
let test2 = (nth_element [1] 0 = Some 1);;
let test3 = (nth_element [1] 1);; (* Exception: Failure "nth" *)
let test4 = (nth_element [1 ; 2] 0 = Some 1);;
let test5 = (nth_element [1 ; 2] 1 = Some 2);;

(* Output

  val nth_element : 'a list -> int -> 'a option = <fun>
  Exception: Failure "nth".
  val test2 : bool = true
  Exception: Failure "nth".
  val test4 : bool = true
  val test5 : bool = true

*)