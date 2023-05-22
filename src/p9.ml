(* Pack Consecutive Duplicates *)

let insert lst x =
  match lst with
  | [] -> [x]::lst
  | a::tl -> (x::a)::tl

let pack lst =
  List.rev (let rec helper lst acc =
    match lst with
    | [] -> acc
    | a::tl -> if (match acc with
      | [] -> false
      | b::tl -> (match b with | [] -> false | x::tl2 -> x = a)
      )
      then helper tl (insert acc a) else helper tl ([a]::acc)
  in
    helper lst []);;

(* Testcases *)
let test1 = pack [];;
let test2 = pack ["a" ; "b" ; "b"];;
let test3 = pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;

(* Output

  val insert : 'a list list -> 'a -> 'a list list = <fun>
  val pack : 'a list -> 'a list list = <fun>
  val test1 : 'a list list = []
  val test2 : string list list = [["a"]; ["b"; "b"]]
  val test3 : string list list =
    [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
    ["e"; "e"; "e"; "e"]]

*)
