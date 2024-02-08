(* Grouping Integers and Strings

   Implement a function `convert` which given

     l : a list of `int_or_string`s

   returns a list of `int_list_or_string_list`s such that adjacent
   `int`s and `string`s are grouped together.

   Example:
   let test_in = [Int 2; Int 3; String "a"; String "b"; Int 4; String "c"]
   let test_out = [IntList [2;3]; StringList ["a";"b"]; IntList [4]; StringList ["c"]]
   let _ = assert (convert test_in = test_out)

*)

type int_list_or_string_list
  = IntList of int list
  | StringList of string list

type int_or_string
  = Int of int
  | String of string

let rec append lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | h :: t -> h :: append t lst2

(*
let rec int_append (lst: int_or_string list) =
  match lst with
  | [] -> []
  | h::t -> (
    match h with
    |Int h -> append [Int h] (int_append t)
    |String h -> []
  )

let rec str_append (lst: int_or_string list) =
  match lst with
  | [] -> []
  | h::t -> (
    match h with
    |String h -> append [String h] (str_append t)
    |Int h -> []
  )
*)
let int_str_list2 (i1 : int_or_string) (i2 : int_or_string) : int_list_or_string_list list=
  match i1, i2 with
  | String n1, String n2 -> [StringList [n1; n2]]
  | Int n1, Int n2 -> [IntList [n1; n2]]
  | Int n1, String n2 -> [IntList [n1]; StringList [n2]]
  | String n2, Int n1 -> [IntList [n1]; StringList [n2]]

  let int_str_list3 (i1 : int_or_string) (i2 : int_list_or_string_list list) : int_list_or_string_list list=
  match i1, i2 with
  | String n1, StringList n2::t-> append t [StringList (append [n1] n2)]
  | Int n1, IntList n2::t -> append t [IntList (append [n1] n2)]
  | Int n1, StringList n2::t -> append t [IntList [n1]; StringList n2]
  | String n1, IntList n2::t -> append t [StringList [n1]; IntList n2]

  let int_str_list4 (i1 : int_list_or_string_list list) (i2 : int_list_or_string_list list) : int_list_or_string_list list=
  match i1, i2 with
  | StringList n1::_, StringList n2::t-> append [StringList (append n1 n2)] t
  | IntList n1::_, IntList n2::t -> append [IntList (append n1 n2)] t
  | IntList n1::_, StringList n2::t -> append [IntList n1; StringList n2] t
  | StringList n1::_, IntList n2::t -> append [StringList n1; IntList n2] t

let convert (l : int_or_string list) : int_list_or_string_list list =
  let rec build lst =
    match lst with
    | [] -> []
    | a::[] -> (match a with
      | Int a1 -> [IntList[a1]]
      | String a1 -> [StringList[a1]])
    | e1::e2::[] -> int_str_list2 e1 e2
    | a::b::c::[] -> int_str_list3 a (int_str_list2 b c)
    | a::b::c::t -> int_str_list4 (int_str_list2 a b) (build (c::t))
  in build l

