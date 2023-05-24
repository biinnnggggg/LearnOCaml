(* Extract a Slice From a List *)
let slice lst i k = 
  let rec helper lst acc index =
    if ( index >= i && index <= k ) then
      ( match lst with
      | [] -> acc
      | x::tl -> helper tl x::acc (index + 1) )
    else
      (if index < i then
        match lst with
        | [] -> acc
        | x::tl -> helper tl acc (index + 1) 
      else acc)
  in List.rev ( helper lst [] 0 )

(* Testcases *)
let test1 = slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;;

(* Output

  val slice : 'a list -> int -> int -> 'a list = <fun>
  val test1 : string list = ["c"; "d"; "e"; "f"; "g"]

*)