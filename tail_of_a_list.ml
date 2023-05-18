(* Tail of a List *)

let rec last alist =
  match alist with
  | [] -> None
  | [a] -> Some a
  | a::tl -> last tl;;


(* Testcases *)
last [];;
last ["a" ; "b"];;
last ["a" ; "b" ; "c"];;