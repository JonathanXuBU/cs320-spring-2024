(* Icy Hot

   Implement the function `reduce` which given

     l : a list of `temp`s

   returns a new list of `temp`s gotten by the following reduction rule:

   If `Hot i` and `Icy i` are adjacent (in particular, they must be
   carrying the same value) in any order, then they cancel out and are
   removed from the list.

   This rule should be carried out until it not possible to reduce the
   list any further.

   Examples:
   let _ = assert (reduce [Hot 0;Icy 0] = [])
   let _ = assert (reduce [Hot 0;Icy 1;Hot 1;Icy 0] = [])
   let _ = assert (reduce [Hot 0;Hot 1;Icy 2;Hot 2;Hot 0;Icy 1] = [Hot 0;Hot 1;Hot 0;Icy 1])

*)
let rec append lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | h :: t -> h :: append t lst2

type temp
  = Hot of int
  | Icy of int

let hot_ice (t1 : temp) (t2: temp) : temp list =
  match t1, t2 with
  | Hot a, Hot b -> [Hot a; Hot b]
  | Icy a, Icy b -> [Icy a; Icy b]
  | Hot a, Icy b -> (if (a = b) then []
  else [Hot a; Icy b])
  | Icy a, Hot b -> (if (a = b) then []
  else [Icy a; Hot b])

let rec cool (l:temp list) : temp list =
  match l with
  | [] -> []
  | [a] -> [a]
  | [a; b] -> hot_ice a b
  | a::b::t -> (append (hot_ice a b) (cool t))
let reduce (l : temp list) : temp list =
  let rec cool_loop (l:temp list) (i:int) =
    match l , i with
    | [], _ -> []
    | _, 0 -> l
    | h::t,_ -> h::cool t
  in cool_loop l (List.length l)