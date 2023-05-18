(* Palindrome *)
let is_palindrome lst =
  (let rec rev lst acc = 
    match lst with
    | [] -> acc
    | a::tl -> rev tl (a::acc)
  in
    rev lst []) = lst;;

(* Testcases *)
let test1 = is_palindrome [];;
let test2 = is_palindrome [1];;
let test3 = is_palindrome [1 ; 1];;
let test4 = is_palindrome [1 ; 2; 1];;
let test5 = is_palindrome [1 ; 2 ; 3];;

(* Output

  val is_palindrome : 'a list -> bool = <fun>
  val test1 : bool = true
  val test2 : bool = true
  val test3 : bool = true
  val test4 : bool = true
  val test5 : bool = false

*)