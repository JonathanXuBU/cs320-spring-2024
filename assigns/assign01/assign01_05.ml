(* Block text

   Please implement the function `block_text` of type `string ->
   string` which, given

   - a string `s` consisting only of capital English characters A-Z
   - a nonnegative integer `min_width`
   - a positive integer `max_width`

   returns a string with the same characters as `s` but separated into
   lines with the following properties:

   - each line has length at most `max_width`
   - each line has the same length except possibly the last line
   - the last line has length at least `min_width`

   If the above three properties are not possible to satisfy, then
   every line except for the last line of the returned string should
   have length `max_width` (in other words, ignore the last
   condition).

   If there are multiple ways to satisfy the above three properties,
   the choice with the longest lines (besides the last line) should be
   used.

   Hint: Take a look at the following functions in the OCaml
   documentation:

   - `String.sub`
   - `String.length`

   Examples:
   let _ = assert (block_text "ABCDEFGHIJ" 0 3 = "ABC\nDEF\nGHI\nJ")
   let _ = assert (block_text "ABCDEFGHIJ" 2 3 = "AB\nCD\nEF\nGH\nIJ")
   let _ = assert (block_text "ABCDEFGHIJ" 0 4 = "ABCD\nEFGH\nIJ")

 *)
let check_width len tn min =
  if (len mod tn) >= min || ((len mod tn) = 0 && tn >= min) then true else false

let rec find_width len maxw minw i k =
  if i <= maxw then
    if check_width len i minw then find_width len maxw minw (i+1) i else find_width len maxw minw (i+1) k
  else k

(*
let rec block_format s width i =
  if i + width >= String.length s then String.sub s i (String.length s - i - 1) 
  else String.sub s i width ^ "/n" ^ block_format s width (i+width)
*)

let block_text (s : string) (min_width : int) (max_width : int) : int =
  (*block_format s (find_width (String.length s) min_width max_width 1 0) 0*)
  find_width (String.length s) max_width min_width 1 0
  
