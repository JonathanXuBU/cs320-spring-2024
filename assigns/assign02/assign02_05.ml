(* Listing Paths

   Implement a function `all_paths` which given

     len : a nonnegative integer
     stp : a point (see below)
     endp : a point

   returns a list of `(dir * int)` lists, where each list contains
   sequence of directions to get from `stp` to `endp` in exactly `len`
   steps.

   Notes:
   A sequence of directions is a list of `dir * int` pairs with the
   property that no adjacent elements have the same `dir`.  For example,

     [(N, 1); (S, 1); (N, 3)]

   is a valid sequence of directions but

     [(N, 1); (N, 3); (S, 1)]

   is not. Instead, this should be

     [(N, 4), (S, 1)]

   Examples:
   let origin = {x=0;y=0}
   let _ = assert (all_paths 0 origin origin = [[]])
   let _ = assert (all_paths 1 origin origin = [])
   let _ = assert (all_paths 2 origin origin =
       [[(N,1);(S,1)] ;
        [(S,1);(N,1)] ;
        [(E,1);(W,1)] ;
        [(W,1);(E,1)] ])
   let _ = assert (all_paths 3 origin origin = [])
   let _ = assert (List.mem [(N,2);(S,2)] (all_paths 4 origin origin))
   let _ = assert (List.mem [(N,1);(E,1);(W,1);(S,1)] (all_paths 4 origin origin))

*)
(*
type dir = N | S | E | W

type point = {
  x : int ;
  y : int ;
}

let rec all_paths (len : int) (stp : point) (endp : point) : (dir * int) list list =
  assert false (* TODO *)
  *)

(*
let op accum next =
  fun n -> max (accum n) (next n)

let base n = 0

let func_max (fs : (int -> int) list) : int -> int =
  List.fold_left op base fs

let l = (func_max [(+) 1; fun x -> x * x] 1)
*)
let x y =
 let x = y in
 let y x = x + 1 in
 let x y = y x in
 x y

let l = (x 2)