(* Matrices

   In this problem you will be building a (very) small interface for
   matrices.  A matrix is represented as a record which keeps track of
   its numbers of rows and columns as well as the values represented
   as a list of rows (i.e., a list of lists).  You will be
   implementing some error handling by working with `result`s.

   ************

   Task 1: Implement a function `mkMatrix` which given

     rs : a list of lists

   returns a matrix represented by this list of lists if it is valid
   or an `error` otherwise.  The error conditions are:

   * If the lengths of the rows in `rs` are not all equal then
   `mkMatrix` should return `Error UnevenRows`.

   * If `rs` is empty then `mkMatrix` should return `Error ZeroRows`.

   * If `rs` contains only empty lists, then `mkMatrix` should reutrn
   `Error ZeroCols`.

   In other words, `mkMatrix` should only return a matrix if `rs`
   represents a nonempty rectangular grid of values.

   Example:
   let l = [[1;2;3];[4;5;6]]
   let rm = mkMatrix l
   let _ = match rm with
     | Ok m ->
       let _ = assert (m.num_rows = 2) in
       let _ = assert (m.num_cols = 3) in
       ()
     | _ -> assert false

   let r = [[1;2;3;4];[1;2;3]]
   let rm' = mkMatrix r
   let _ = match rm' with
     | Ok _ -> assert false
     | Error e -> assert (e = UnevenRows)

   ************

   Task 2: Implement the function `transpose` which, given

     m : a matrix

   returns the transpose of `m`.

   Example:
   let _ = match rm with
     | Ok m ->
       let tm = transpose m in
       let _ = assert (tm.num_rows = 3) in
       let _ = assert (tm.num_cols = 2) in
       let _ = assert (tm.rows = [[1;4];[2;5];[3;6]]) in
       ()
     | _ -> assert false

   ************

   Task 3: Implement the function `multiply` which, given

     m : a matrix
     n : a matrix

   returns the product of `m` and `n` if it is possible to multiply
   them. The error condition:

   * If the number of columns of `m` is not the same as the number of
   rows of `n`, then `multiply` should return `Error MulMismatch`

   Example:
   let a =
     { num_rows = 2 ;
       num_cols = 2 ;
       rows = [[1.;2.];[3.;4.]] ;
     }

   let b =
     { numn_rows = 1 ;
       num_cols = 2 ;
       rows = [[1.; 2.]] ;
     }

   let _ = assert (multiply a a = Ok {
     num_rows = 2 ;
     num_cols = 2 ;
     rows = [[7.;10.];[15.;22.]] ;
    })

   let _ = assert (multiply a b = Error MulMismatch)

   ************

   References:
   https://en.wikipedia.org/wiki/Matrix_multiplication
   https://en.wikipedia.org/wiki/Transpose
   https://www.cs.bu.edu/fac/crovella/cs132-book/landing-page.html

*)

type error
   = UnevenRows
   | ZeroRows
   | ZeroCols
   | MulMismatch

type 'a matrix = {
  num_rows : int ;
  num_cols : int ;
  rows : ('a list) list ;
}

let rec append (l1: 'a list) (l2 : 'a list) : 'a list=
  match l1 with
  | [] -> l2
  | h :: t -> h :: append t l2

let rec checkEven (r1: 'a list) (r2: 'a list): bool =
  match r1, r2 with
  | [], [] -> true
  | [], _ -> false
  | _, [] -> false
  | h1::t1, h2::t2 -> checkEven t1 t2

let rec checkRows (rs: 'a list list): bool =
  match rs with
  | [] -> true
  | (h::[]) -> true
  | (h::t) ->
    (match t with
    | (hh::tt) -> if (checkEven h hh) then
      checkRows(t) else false
    | _ -> assert false
    )

let rec isEmptyCols (rs: 'a list list): bool =
  match rs with
  | [] -> true
  | (h::t) -> (
      match h with
      | [] -> isEmptyCols t
      | _ -> false
      )

let dimension (m : 'a list list) : (int * int) =
  let no_rows = List.length m
  in let no_cols =
    match m with
    | [] -> 0
    | r :: t -> List.length r
  in (no_rows,no_cols)
(*taken from lecture*)

let createMatrix (rs: 'a list list) : 'a matrix =
  match (dimension rs) with
  | (row_no, col_no) -> {num_rows = row_no; num_cols = col_no; rows = rs}


let mkMatrix (rs : 'a list list) : ('a matrix, error) result =
  match rs with
  | [] -> Error ZeroRows
  | _ -> (if (isEmptyCols rs) then
    Error ZeroCols else
      (if checkRows rs then
      (Ok (createMatrix rs)) else
      Error UnevenRows
  )
)

let rec first_column : 'a list list -> 'a list list option = function
  | [] -> None
  | h :: t -> match h with
    | [] -> None 
    | hh :: tt -> match first_column t with
      | None -> Some [[hh]]
      | Some ttt -> Some ([hh] :: ttt)

let rec rest_columns : 'a list list -> 'a list list option = function
  | [] -> None
  | h :: t -> match h with
    | [] -> None 
    | hh :: tt -> match rest_columns t with
      | None -> Some [tt]
      | Some ttt -> Some (tt :: ttt)

let rec collapse (lst : 'a list list) : 'a list =
  match lst with
  | [] -> []
  | h::t -> append h (collapse (t))

let rec transpost (rs : 'a list list) : 'a list list =
  match rs with
  | [] -> []
  | l -> (if isEmptyCols rs then [] else
     (
    match first_column rs, rest_columns rs with
    | Some h, Some t -> append [collapse h] (transpost t)
    | _,_ -> assert false
  )
  )

let transpose (m : 'a matrix) : 'a matrix =
  {num_rows = m.num_cols; num_cols = m.num_rows; rows = transpost m.rows}

let multiply (m : float matrix) (n : float matrix) : (float matrix, error) result =
  assert false (* TODO *)


(*
let l = [[1;2;3];[4;5;6];[1;2;3];[4;5;6];[1;2;3];[4;5;6]]
let lst2 = [[1;2;3];[4;6]]
let lst3 = [[];[];[];[]]
let lst 4 = []
let l1 = [1; 2 ; 3]
let l2 = [1; 5 ; 3]
let l3 = [1; 5 ; 3; 5]
let l4 = [1; 5]

*)

let l = [[1;2;3];[4;5;6]]
let rm = mkMatrix l
let _ = match rm with
  | Ok m ->
    let _ = assert (m.num_rows = 2) in
    let _ = assert (m.num_cols = 3) in
    ()
  | _ -> assert false

let r = [[1;2;3;4];[1;2;3]]
let rm' = mkMatrix r
let _ = match rm' with
  | Ok _ -> assert false
  | Error e -> assert (e = UnevenRows)

let _ = match rm with
| Ok m ->
  let tm = transpose m in
  let _ = assert (tm.num_rows = 3) in
  let _ = assert (tm.num_cols = 2) in
  let _ = assert (tm.rows = [[1;4];[2;5];[3;6]]) in
  ()
| _ -> assert false