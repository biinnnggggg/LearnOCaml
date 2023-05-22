(* Eliminate Duplicates *)

let rec contains lst x =
  match lst with
  | [] -> false
  | a::tl -> if a = x then true else contains tl x;;

let compress lst = 
  List.rev (let rec helper lst acc =
    match lst with
    | [] -> acc
    | a::tl -> if (contains acc a) then helper tl acc else helper tl (a::acc)
  in helper lst []);;

(* Testcases *)
let test1 = compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
let test2 = compress [];;
let test3 = compress ["a"];;

(* Output

  val contains : 'a list -> 'a -> bool = <fun>
  val compress : 'a list -> 'a list = <fun>
  val test1 : string list = ["a"; "b"; "c"; "d"; "e"]
  val test2 : 'a list = []
  val test3 : string list = ["a"]  

*)