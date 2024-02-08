(* Recipes by Ingredients

   Implement a function `recs_by_ingrs` which given

     recs : a list of recipes
     ingrs : a list of ingredients (i.e., strings)

   returns the list of those recipes in `recs` (in the same order)
   whose ingredients are included in `ingrs`.

   You may assume that `ingrs` and `r.ingrs` for every `r` in `recs`
   do not contain duplicates.

   Hint: The function List.mem may be useful.

   Example:
   let r1 = { name = "1" ; ingrs = ["a"; "b"; "d"] }
   let r2 = { name = "2" ; ingrs = ["a"; "c"; "e"] }
   let r3 = { name = "3" ; ingrs = ["b"; "c"] }
   let _ = assert (recs_by_ingrs [r1;r2;r3] ["a";"b";"c";"d"] = [r1;r3])
   let _ = assert (recs_by_ingrs [r1;r2;r3] ["a";"b";"c";"e"] = [r2;r3])

*)

type ingr = string

type recipe = {
  name : string ;
  ingrs : ingr list;
}

let rec append lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | h :: t -> h :: append t lst2

let rec check_recipe (hing : ingr list) (ingr : ingr list) : bool =
  match hing with
  | [] -> true
  | h::t -> if (List.mem h ingr) then
    true && check_recipe (t) (ingr) else
    false


let recs_by_ingrs (l : recipe list) (s : ingr list) : recipe list =
  let rec recipes (lst : recipe list) (ingr :ingr list) =
    match lst with
    | [] -> []
    | h::t -> if check_recipe h.ingrs ingr then
      append [h] (recipes t ingr) else
      (recipes t ingr)
    in recipes l s
