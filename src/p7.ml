(* Flatten a List *)

(* type definition *)
type 'a node =
  | One of 'a
  | Many of 'a node list

(* function definition *)
let flatten lst = 
  List.rev (let rec helper lst acc =
    match lst with
    | [] -> acc
    | (One x)::tl -> helper tl (x::acc)
    | (Many x)::tl -> helper tl (helper x acc)
    in helper lst []);;

(* Testcases *)
let test1 = flatten [];;
let test2 = flatten [One "a"; Many [One "b"; Many [One "c" ; One "d"]; One "e"]];;

(* Output

  type 'a node = One of 'a | Many of 'a node list
  val flatten : 'a node list -> 'a list = <fun>
  val test1 : 'a list = []
  val test2 : string list = ["a"; "b"; "c"; "d"; "e"]

*)