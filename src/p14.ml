(* Duplicate the Elements of a List *)
let duplicate lst =
  let rec aux lst acc =
    match lst with
    | [] -> acc
    | a::tl -> aux tl (a::(a::acc))
  in aux ( List.rev lst ) [];;

(* Testcases *)
let test1 = duplicate ["a"; "b"; "c"; "c"; "d"];;

(* Output

val duplicate : 'a list -> 'a list = <fun>
val test1 : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]

*)