(* Taxicab numbers

   A taxicab number is a positive integer which can be expressed as
   the sum of two positive cubes in more than one way. (This is
   slightly different than the usual definition.)

   Please implement the function `taxicab` of type `int -> int` which,
   given a positive integer `n`, returns the the number of ways that
   `n` can be expressed as the sum of two positive cubes.

   Examples:
   let _ = assert (taxicab 2 = 1)
   let _ = assert (taxicab 5 = 0)
   let _ = assert (taxicab 1729 = 2)   (* 1729 = 1^3 + 12^3 = 9^3 + 10^3 *)
   let _ = assert (taxicab 4104 = 2)   (* 4104 = 2^3 + 16^3 = 9^3 + 15^3 *)

 *)

 let cube n = n * n * n

let rec max_cube m c =
  if cube c > m/2 then 0 else (1 + max_cube m (c+1))

let check_tax n i j =
  if cube i + cube j = n then true else false
  
let rec taxi n i j m =
    if i > m then 0 else
      if cube j > n then taxi n (i+1) 0 m else
        if check_tax n i j then 1 + taxi n i (j+1) m
        else 0 + taxi n i (j+1) m

let taxicab (n : int) : int =
  taxi n 0 0 ((max_cube n 1));
