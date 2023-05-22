(* Decodea Run-length Encoded List *)

type 'a rle =
  | One of 'a
  | Many of int * 'a;;


let decode lst = 
  let insert lst item =
    let rec aux item acc =
      match item with
      | One x -> x::acc
      | Many (n, x) -> if n = 0 then acc else aux (Many (n - 1, x)) (x::acc)
    in aux item lst
  in
    List.rev (let rec helper lst acc = 
                match lst with
                | [] -> acc
                | a::tl -> helper tl (insert acc a)
              in
                helper lst [])

(* Testcases *)
let test1 = decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;

(* Output

  type 'a rle = One of 'a | Many of int * 'a
  val decode : 'a rle list -> 'a list = <fun>
  val test1 : string list =
    ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]

*)