(* Split a List Into Two Parts; The Length of the First Part is Given*)
let split lst n = 
  let rec aux lst n acc = 
    match n with
    | 0 -> (List.rev acc, lst)
    | x -> match lst with
            | [] -> (List.rev acc, lst)
            | a::tl -> aux tl (n - 1) (a::acc)
  in
    aux lst n [];;

(* Testcases *)
let test1 = split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
let test2 = split ["a"; "b"] 0;;

(* Output

  val split : 'a list -> int -> 'a list * 'a list = <fun>
  val test1 : string list * string list =
    (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
  val test2 : string list * string list = ([], ["a"; "b"])

*)