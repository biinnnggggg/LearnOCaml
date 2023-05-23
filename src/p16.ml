(* Drop Every N'th Element From a List *)
let drop lst n =
  let rec aux lst acc x =
    match lst with
    | [] -> acc
    | a::tl -> if x = n then aux tl acc 1 else aux tl (a::acc) (x + 1) in
  List.rev (aux lst [] 1);;

(* Testcases *)
let test1 = drop ["a"; "b"] 1;;
let test2 = drop ["a"; "b"] 2;;
let test3 = drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;

(* Output

  val drop : 'a list -> int -> 'a list = <fun>
  val test1 : string list = []
  val test2 : string list = ["a"]
  val test3 : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]

*)