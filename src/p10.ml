(* Run-Length Encoding *)

let insert lst x =
  match lst with
  | [] -> (1, x)::lst
  | a::tl -> (match a with | n, y -> if x = y then (n + 1, x)::tl else (1, y)::lst)

let encode lst =
  List.rev (let rec helper lst acc =
    match lst with
    | [] -> acc
    | a::tl -> helper tl (insert acc a)
  in
    helper lst []);;


(* Testcases *)
let test1 = encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;

(* Output

  val insert : (int * 'a) list -> 'a -> (int * 'a) list = <fun>
  val encode : 'a list -> (int * 'a) list = <fun>
  val test1 : (int * string) list =
    [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]

*)