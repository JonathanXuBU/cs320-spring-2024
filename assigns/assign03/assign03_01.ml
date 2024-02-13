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

let rec append (l1: 'a list) (l2 : 'a list) : 'a list=
  match l1 with
  | [] -> l2
  | h :: t -> h :: append t l2

let rec sortlist lst =
  match lst with
    [] -> []
  | head :: tail -> insert head (sortlist tail)
and insert elt lst =
  match lst with
    [] -> [elt]
  | head :: tail -> if elt <= head then elt :: lst else head :: insert elt tail;;

type 'a concatlist
  = Nil
  | Single of 'a
  | Concat of 'a concatlist * 'a concatlist

let sort (l : 'a concatlist) : 'a list =
  let rec go (e: 'a concatlist) :'a list =
    match e with
    | Nil -> []
    | Single a -> [a]
    | Concat (a,b) -> sortlist (append (go a) (go b))
  in go l

