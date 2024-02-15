(* Concatenation Lists

   A `concatlist` is a list whose constructors are based on
   concatenation instead of cons.  It has a constructor for the empty
   list (Nil), a single element list (Single) and the concatentation
   of two lists (Concat).

   Implement a function `sort` which given

     l : a `concatlist`

   returns a regular list with the same element as `l` but in sorted
   order.  You should do this WITHOUT trying to first converting `l`
   into a regular list.  In particular, you CANNOT use the function
   `List.sort`.

   Example:
   let l = Concat (Concat (Single 3, Single 2), Concat (Single 1, Single 10))
   let _ = assert (sort l = [1;2;3;10])

*)

(*
let rec append (l1: 'a list) (l2 : 'a list) : 'a list=
  match l1 with
  | [] -> l2
  | h :: t -> h :: append t l2

let rec sortlist lst =
  match lst with
    [] -> []
  | head :: tail -> insert head (sortlist tail)
  (*iterates through the list for each element*)
and insert elem lst =
  match lst with
    [] -> [elem]
  | head :: tail -> if elem <= head then elem :: lst else head :: insert elem tail;;
  (*takes that element and compares it to the recursive call of sort on the rest of the elements,
     comparing them and putting into place depending on whether it is greater than or less than the next element*)
(*Code referenced from a post on Stack Exchange, Comments added to demonstrate my understanding
   and deconstruction*)
  
*)

let rec mergesort l1 l2 =
  match l1, l2 with
  | [], _ -> l2
  | _, [] -> l1
  | h :: t, hh :: tt ->
      if h < hh then h :: mergesort t l2 else hh :: mergesort l1 tt

type 'a concatlist
  = Nil
  | Single of 'a
  | Concat of 'a concatlist * 'a concatlist

let sort (l : 'a concatlist) : 'a list =
  let rec go (e: 'a concatlist) :'a list =
    match e with
    | Nil -> []
    | Single a -> [a]
    | Concat (a,b) -> mergesort (go a) (go b)
  in go l

