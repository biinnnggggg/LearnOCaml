(* Modified Run-Length Encoding *)

type 'a rle =
  | One of 'a
  | Many of int * 'a;;

  let insert lst x =
    match lst with
    | [] -> (One x)::lst
    | (One y)::tl -> if y = x then (Many (2, y))::tl else (One x)::lst 
    | (Many (n, y))::tl -> if y = x then (Many (n + 1, y))::tl else (One x)::lst;;
  
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

  type 'a rle = One of 'a | Many of int * 'a
  val insert : 'a rle list -> 'a -> 'a rle list = <fun>
  val encode : 'a list -> 'a rle list = <fun>
  val test1 : string rle list =
    [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
    Many (4, "e")]

*)