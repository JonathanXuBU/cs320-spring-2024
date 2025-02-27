(* Reversing strings

   Please implement the function `string_rev` of type `string ->
   string` which, given a string `s`, returns the string with the same
   characters but in reverse order.

   Hint: Recall that there are no built-in functions for converting
   from `string` to `char or vice versa. See OCP 2.3.1 for details on
   how to accomplish this.
ls
   Hint: Take a look at the following functions in the OCaml
   documentation:

   - `String.sub`
   - `String.length`

   Examples:
   let _ = assert (string_rev "testing" = "gnitset")
   let _ = assert (string_rev "12345" = "54321")
   let _ = assert (string_rev "noon" = "noon")

 *)


let rec reverse_string(s: string) (n: int) = 
    if (n : int) > String.length s then String.empty else String.sub s (String.length s - n) 1 ^ reverse_string s (n+1)

let string_rev (s : string) : string =
  reverse_string s 1