(* Tail of a List *)
let rec last alist =
  match alist with
  | [] -> None (* if the list is empty then there is no last element *)
  | [a] -> Some a (* if the last element it reached return *)
  | a::tl -> last tl;; (* recurse on the tail of the list *)

(* Testcases *)
last ["a" ; "b" ; "c"];;