(* Reverse a List *)
let rev lst =
  let rec helper lst acc = 
    match lst with
    | [] -> acc
    | a::tl -> helper tl (a::acc)
  in
    helper lst [];;

(* Testcases *)
let test1 = (rev [] = []);;
let test2 = (rev [1] = [1]);;
let test3 = (rev [1 ; 2 ; 3 ; 4 ; 5] = [5 ; 4 ; 3 ; 2 ; 1]);;

(* Output

  val rev : 'a list -> 'a list = <fun>
  val test1 : bool = true
  val test2 : bool = true
  val test3 : bool = true

*)