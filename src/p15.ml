(* Replicate the Elements of a List a Given Number of Times *)

let replicate lst n =
  let rec prepend lst x n =
    if n = 0 then lst else prepend (x::lst) x (n - 1) in
  let rec aux lst acc =
    match lst with
    | [] -> acc
    | a::tl -> aux tl (prepend acc a n) in
  aux ( List.rev lst ) [];;

(* Testcases *)
let test1 = replicate ["a"; "b"; "c"] 3;;

(* Output
    
  val insert : 'a list -> 'a -> int -> 'a list = <fun>
  val replicate : 'a list -> int -> 'a list = <fun>
  val test1 : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]

*)