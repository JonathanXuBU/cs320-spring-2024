(* Boolean Expressions

   Implement a function `eval` which given

     v: an association list mapping `string`s to `bool`s
     e: a boolean expression (see below)

   returns `Some b` if every variable in `e` also appears in `v` and
   `e` evaluates to `b`, or `None` if not all variable in `e` appear
   in `v`.

   Example:
   let v = [("a", true); ("b", false); ("c", true)]
   let e = And (Var "a", Or (Var "b", Var "c"))
   let f = Not (Var "d")
   let _ = assert (eval v e = Some true)
   let _ = assert (eval v f = None)

   One way to think about evaluation: imagine `v` as defining a
   collection of `bools` in OCaml:

     let a = true
     let b = false
     let c = true

   and an expression as defining a boolean expression in OCaml:

     let e = a && (b || c)

   The goal of evaluation is to determine the value of `e`.

   Likewise, if you define an expression with a name that has not
   been defined, you would get a compile-time error

     let f = not d

   which is why the function `eval` should return `None` on `f`.

   Hint: Take a look at the textbook section on association lists
   (they are a simple implementation of a dictionary-like data
   structure), as well as the function List.assoc_opt.

*)

type bexp =
  | Var of string
  | Not of bexp
  | And of bexp * bexp
  | Or of bexp * bexp

let eval (v : (string * bool) list) (e : bexp) : bool option =
  let rec bool_eval (lst:(string * bool) list) (ex: bexp) =
  match ex with
  | Var exp -> List.assoc_opt exp v
  | Not exp -> 
    (match bool_eval (v) (exp) with
    | Some true -> Some false
    | Some false -> Some true
    | None -> None
    )
  | And (exp, exp1) ->  
    (match (bool_eval lst (exp), bool_eval lst (exp1)) with
    | _, None -> None
    | None, _ -> None
    | Some a, Some b -> Some (a && b)
    )
  | Or (exp, exp1) ->
    (match (bool_eval lst (exp), bool_eval lst (exp1)) with
    | _, None -> None
    | None, _ -> None
    | Some a, Some b -> Some (a || b)
    )
  in bool_eval v e 