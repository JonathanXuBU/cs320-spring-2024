(* UTILITIES *)
let cons x xs = x :: xs
let explode s = List.of_seq (String.to_seq s)
let implode cs = String.of_seq (List.to_seq cs)
let is_digit c = '0' <= c && c <= '9'
let is_blank c = String.contains " \012\n\r\t" c
let is_upper_case c = 'A' <= c && c <= 'Z'
let is_lower_case c = 'a' <= c && c <= 'z'

type 'a parser = char list -> ('a * char list) option

let satisfy f = function
  | c :: cs when f c -> Some (c, cs)
  | _ -> None

let char c = satisfy ((=) c)

let str s cs =
  let rec go = function
    | [], ds -> Some (s, ds)
    | c :: cs, d :: ds when c = d -> go (cs, ds)
    | _ -> None
  in go (explode s, cs)

let map f p cs =
  match p cs with
  | Some (x, cs) -> Some (f x, cs)
  | None -> None

let (>|=) p f = map f p
let (>|) p x = map (fun _ -> x) p

let seq p1 p2 cs =
  match p1 cs with
  | Some (x, cs) -> (
      match p2 cs with
      | Some (y, cs) -> Some ((x, y), cs)
      | None -> None
    )
  | None -> None

let (<<) p1 p2 = map fst (seq p1 p2)
let (>>) p1 p2 = map snd (seq p1 p2)

let map2 f p1 p2 =
  seq p1 p2 >|= fun (x, y) -> f x y

let optional p cs =
  match p cs with
  | Some (x, cs) -> Some (Some x, cs)
  | None -> Some (None, cs)

let rec many p cs =
  match p cs with
  | Some (x, cs) -> (
      match (many p cs) with
      | Some (xs, cs) -> Some (x :: xs, cs)
      | None -> Some ([x], cs)
    )
  | None -> Some ([], cs)

let many1 p = map2 cons p (many p)

let alt p1 p2 cs =
  match p1 cs with
  | Some x -> Some x
  | None ->
    match p2 cs with
    | Some x -> Some x
    | None -> None

let (<|>) = alt

let pure x cs = Some (x, cs)
let fail _ = None

let bind p f cs =
  match p cs with
  | Some (x, cs) -> f x cs
  | None -> None

let (>>=) = bind
let ( let* ) = bind

let choice ps =
  List.fold_left (<|>) fail ps

let ws = many (satisfy is_blank)
let keyword w = str w << ws

let rec_parser p =
  pure () >>= p

let parse p s =
  match p (explode s) with
  | Some (x, []) -> Some x
  | _ -> None

(* END OF UTILITIES *)

(* REQUIRED TYPES *)

type ident = string

type const
  = Num of int
  | Bool of bool

type command
  = Push of const | Trace
  | Add | Mul | Div
  | And | Or | Not | Lt | Eq
  | If of program * program
  | While of program * program
  | Bind of ident | Fetch of ident
  | Fun of program | Call | Return
  | Debug of string

and program = command list

and bindings = (ident * value) list

and value
  = Const of const
  | Clos of
      { def_id : int
      ; captured : bindings
      ; prog : program
      }

type record =
  { id : int
  ; local : bindings
  ; called_def_id : int
  ; return_prog : program
  }

type stack = value list
type trace = string list
type env
  = Global of bindings
  | Local of record * env

(* get the id of the topmost record *)
let local_id = function
  | Global _ -> 0
  | Local (r, _) -> r.id

(* convert a value to a string *)
let to_string v =
  match v with
  | Const (Bool true) -> "True"
  | Const (Bool false) -> "False"
  | Const (Num n) -> string_of_int n
  | Clos _ -> "<Closure>"

(* PARSING *)

let parse_ident =
  map2
    (fun c cs -> implode (c :: cs))
    (satisfy is_lower_case)
    (many (satisfy (fun c -> is_lower_case c || is_digit c || c = '_')))

let parse_int =
  let mk_int sign cs =
    let abs = int_of_string (implode cs) in
    if Option.is_none sign
    then abs
    else -abs
  in
  map2
    mk_int
    (optional (char '-'))
    (many1 (satisfy is_digit))

let parse_bool =
  (str "True" >| true) <|> (str "False" >| false)

let parse_comment =
  char '(' >> many (satisfy ((<>) ')')) >> char ')' >| ()

let parse_debug =
  char '"' >> many (satisfy ((<>) '"')) << char '"' >|= implode

let parse_const =
  (parse_bool >|= (fun s -> Bool (s))) <|> (parse_int >|= (fun s -> Num (s)))

let ws = many (ws >> parse_comment) >> ws
let keyword w = str w << ws

let rec parse_com () =
  let parse_fun =
    let* _ = keyword ":" in
    let* body = parse_prog_rec () in
    let* _ = keyword ";" in
    pure (Fun (body))
  in
  let parse_if =
    let* _ = keyword "?" in
    let* ifc = parse_prog_rec () in
    let* _ = keyword ";" in
    let* elsec = parse_prog_rec () in
    let* _ = char ';' in
    pure (If (ifc, elsec))
  in
  let parse_while =
    let* _ = keyword "While" in
    let* check = parse_prog_rec () in
    let* _ = keyword ";" in
    let* body = parse_prog_rec () in
    let* _ = char ';' in
    pure (While (check, body))
  in
  choice
    (* TODO: Add more alternatives *)
    [ parse_fun
    ; parse_const >|= (fun s -> Push (s))
    ; (keyword "+" >| Add)
    ; (keyword "*" >| Mul)
    ; (keyword "/" >| Div)
    ; (keyword "&&" >| And)
    ; (keyword "||" >| Or)
    ; (keyword "~" >| Not)
    ; (keyword "<" >| Lt)
    ; (keyword "=" >| Eq)
    ; parse_while
    ; parse_if
    ; parse_ident >|= (fun s -> Fetch s)
    ; parse_debug >|= (fun s -> Debug s)
    ; keyword "|>" >> parse_ident >|= (fun s -> Bind s)
    ; (keyword "#" >| Call)
    ; (keyword "." >| Trace)
    ; (keyword "Return" >| Return)
    ]
and parse_prog_rec () =
  many (rec_parser parse_com << ws)

let parse_prog = parse (ws >> parse_prog_rec ())

(* A VERY SMALL TEST SET *)
let test = parse_prog "Return"
let out = Some [Return]
let _ = assert (test = out)

let test = parse_prog "    || "
let out = Some [Or]
let _ = assert (test = out)

let test = parse_prog "  |> test   "
let out = Some [Bind "test"]
let _ = assert (test = out)


(* FETCHING AND UPDATING *)

(* fetch the value of `x` in the environment `e` *)
let fetch_env e x = 
  let rec fetch_binding (x:ident) (blist: bindings) =
    (*fetches bind given a bindings list*)
    match blist with
    | [] -> None
    | h::t -> (
      match h with
      | (ident, v) -> (
        if (ident = x) then Some v else (fetch_binding x t)
      )
    )
  in
  let rec check_id (e:env) (x:ident) (id:int) =
    (*checks for a specific indent given a specific id*)
    match e with
    | Global b -> fetch_binding x b
    | Local (r, rest_env) ->
      (
        if (r.id = id) then (
          match (fetch_binding x r.local) with
          | Some v -> Some v
          | None -> 
            (match e with 
            | Global _ -> None
            | Local (r, r_env) -> (
              check_id e x r.called_def_id
              )
            )
        )
      else check_id rest_env x id
      )
  in check_id e x (local_id(e))
  (*runs check id on local variable -> call id*)

  


let rec update_env e x v = 
  match e with
  | Global gv ->
      Global ((x, v) :: gv)
  | Local (r, rest_env) ->
      Local ({id = r.id ; local = (x, v):: r.local; called_def_id = r.called_def_id; return_prog = r.return_prog}, rest_env)

(* EVALUTION *)

let test = (Local ({id = 2; local = [("w", Const (Num 1));("q", Const (Num 2))]; called_def_id = 1; return_prog = [Return]},
 Local ({id = 1; local = [("x", Const (Num 5));("y", Const (Num 4))]; called_def_id = 0; return_prog = [Return]},
Global [("z", Const (Num 10)); ("g", Const (Num 20))]))
)
let _ = fetch_env test "w"
(* make the panic configuration given a configuration *)
let panic (_, _, t, _) msg = [], Global [], ("panic: " ^ msg) :: t, []

let eval_step (c : stack * env * trace * program) =
  match c with
  (* Push *)
  | s, e, t, Push c :: p -> Const c :: s, e, t, p
  | s, e, t, Fun f :: p -> Clos {def_id = local_id(e); captured = []; prog = f} :: s, e, t, p
  (* Trace *)
  | v :: s, e, t, Trace :: p -> s, e, to_string v :: t, p
  | [], _, _, Trace :: _ -> panic c "stack underflow (. on empty)"
  (* Add *)
  | Const (Num m) :: Const (Num n) :: s, e, t, Add :: p -> Const (Num (m + n)) :: s, e, t, p
  | _ :: _ :: _, _, _, Add :: _ -> panic c "type error (+ on non-integers)"
  | _ :: [], _, _, Add :: _ -> panic c "stack underflow (+ on single)"
  | [], _, _, Add :: _ -> panic c "stack underflow (+ on empty)"
  (*Multiply*)
  | Const (Num m) :: Const (Num n) :: s, e, t, Mul :: p -> Const (Num (m * n)) :: s, e, t, p
  | _ :: _ :: _, _, _, Mul :: _ -> panic c "type error (* on non-integers)"
  | _ :: [], _, _, Mul :: _ -> panic c "stack underflow (* on single)"
  | [], _, _, Mul :: _ -> panic c "stack underflow (* on empty)"
  (*Divide*)
  | Const (Num m) :: Const (Num 0) :: s, e, t, Div :: p -> panic c "divByZero error (/ on 0)"
  | Const (Num m) :: Const (Num n) :: s, e, t, Div :: p -> Const (Num (m / n)) :: s, e, t, p
  | _ :: _ :: _, _, _, Div :: _ -> panic c "type error (/ on non-integers)"
  | _ :: [], _, _, Div :: _ -> panic c "stack underflow (/ on single)"
  | [], _, _, Div :: _ -> panic c "stack underflow (/ on empty)"
  (*And*)
  | Const (Bool m) :: Const (Bool n) :: s, e, t, And :: p -> Const (Bool (m && n)) :: s, e, t, p
  | _ :: _ :: _, _, _, And :: _ -> panic c "type error (And on non-bools)"
  | _ :: [], _, _, And :: _ -> panic c "stack underflow (And on single)"
  | [], _, _, And :: _ -> panic c "stack underflow (And on empty)"
  (*Or*)
  | Const (Bool m) :: Const (Bool n) :: s, e, t, Or :: p -> Const (Bool (m || n)) :: s, e, t, p
  | _ :: _ :: _, _, _, Or :: _ -> panic c "type error (Or on non-bools)"
  | _ :: [], _, _, Or :: _ -> panic c "stack underflow (Or on single)"
  | [], _, _, Or :: _ -> panic c "stack underflow (Or on empty)"
  (*Not*)
  | Const (Bool m) :: s, e, t, Not :: p -> Const (Bool (not m)) :: s, e, t, p
  | _ :: _, _, _, Not :: _ -> panic c "type error (Not on non-bools)"
  | [], _, _, Not :: _ -> panic c "stack underflow (Not on empty)"
  (*Less Than*)
  | Const (Num m) :: Const (Num n) :: s, e, t, Lt :: p -> Const (Bool (m < n)) :: s, e, t, p
  | _ :: _ :: _, _, _, Lt :: _ -> panic c "type error (Lt on non-integers)"
  | _ :: [], _, _, Lt :: _ -> panic c "stack underflow (Lt on single)"
  | [], _, _, Lt :: _ -> panic c "stack underflow (Lt on empty)"
  (*Equals*)
  | Const (Num m) :: Const (Num n) :: s, e, t, Eq :: p -> Const (Bool (m = n)) :: s, e, t, p
  | _ :: _ :: _, _, _, Eq :: _ -> panic c "type error (Eq on non-integers)"
  | _ :: [], _, _, Eq :: _ -> panic c "stack underflow (Eq on single)"
  | [], _, _, Eq :: _ -> panic c "stack underflow (Eq on empty)"
  (*If-Else*)
  | Const (Bool true) :: s, e, t, If (ifc, elsec) :: p -> s, e, t, ifc @ p
  | Const (Bool false) :: s, e, t, If (ifc, elsec) :: p -> s, e, t, elsec @ p
  | _ :: _, _, _, If (ifc, elsec) :: _ -> panic c "type error (If-Else on non-bool)"
  | [], _, _, If (ifc, elsec) :: _ -> panic c "stack underflow (If_else on empty)"
  (*While*)
  | s, e, t, While (whilec, thenc) :: p -> s, e, t, whilec @ [If (thenc @ [While (whilec, thenc)], [])] @ p
  (*Debug*)
  | s, e, t, Debug (str) :: p -> s, e, str :: t, p
  (*Fetch*)
  | s, e, t, Fetch (ident) :: p -> (
    match fetch_env e ident with
    | None -> panic c "fetch not found"
    | Some v -> v :: s, e, t, p
  )
  (*Assign*)
  | v :: s, e, t, Bind (id) :: p -> s, (update_env e id v), t, p
  | [] , e, t, Bind (id) :: p -> panic c "bind not found"
  (*Call*)
  | Clos r :: s , e, t, Call :: p -> s, Local ({id = local_id(e) + 1; local = r.captured; called_def_id = r.def_id; return_prog = p } , e) , t, r.prog
  | x :: s, e, t, Call :: p -> panic c "type error (Call on non-closure)"
  | [], e, t, Call :: p -> panic c "stack underflow (Call on empty)"
  (*Return*)
  | Clos r :: [] , Local (v, e), t, Return :: p -> (if r.def_id = v.id then
    (Clos {def_id = v.id; captured = r.captured @ v.local; prog = r.prog} :: [], e, t , v.return_prog)
  else (Clos r :: [], e, t, v.return_prog))
  | x :: [], Local(v, e), t, Return :: p -> x :: [], e, t, v.return_prog
  | [], Local(v, e), t, Return :: p -> [], e, t, v.return_prog
  | [], Local(v, e), t, [] -> [], e, t, v.return_prog
  | x :: y:: s, Local(v, e), t, Return :: p -> panic c "retError1"
  | x :: s, Local(v, e), t, [] -> panic c "retError2"
  | s, Global _ , t, Return :: p -> panic c "retError3"
  | _ -> assert false

let test = "(sqrt): |> n
(if) 0 n < ? Return ; ;
(if) 0 n = ? 0 Return ; ;
0 |> i
While n 1 + i < ;
  (if) i i * n < ?
    i -1 + Return ;
  (else)
    i 1 + |> i
  ;
;
; |> sqrt

(is_prime): |> n
(if) 2 n < ? False Return ; ;
2 |> i
n sqrt # |> s
While s 1 + i < ;
  (if) i n / i * n = ?
    False Return ;
  (else)
    i 1 + |> i
  ;
;
True Return
; |> is_prime

15261281789861 is_prime # .
"
(*
let test = 
"-1 |> x
2 |> y
(g) :
1 |> x
; |> g
(f):
  0 |> x
  0 |> y
  g #
  3 |> x
; |> f
f #
2 |> y"
*)

(*
let Some p = parse_prog test
let c = ([], Global [], [], p)
*)

let rec eval c =
  match c with
  | (_, Global _, t, []) -> t
  | _ -> eval (eval_step c)

let rec eval_prog p = eval ([], Global [], [], p)
let interp s = Option.map eval_prog (parse_prog s)

(* MAIN *)

let print_trace t =
  let rec go t =
    match t with
    | [] -> ()
    | x :: t ->
      print_endline x;
      go t
  in go (List.rev t)

let main () =
  let input =
    let rec get_input s =
      try
        get_input (s ^ "\n" ^ read_line ())
      with End_of_file ->
        s
    in get_input ""
  in
  match interp input with
  | None -> print_endline "Parse Error"
  | Some t -> print_trace t

let _ = main ()

(* END OF FILE *)
