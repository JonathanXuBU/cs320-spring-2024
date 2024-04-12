(* UTILITIES *)
let cons x xs = x :: xs
let explode s = List.of_seq (String.to_seq s)
let implode cs = String.of_seq (List.to_seq cs)
let is_digit c = '0' <= c && c <= '9'
let is_blank c = String.contains " \012\n\r\t" c
let is_upper_case c = 'A' <= c && c <= 'Z'

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

(* ============================================================ *)

(* BEGINNING OF PROJECT CODE *)

type ident = string
type command
  = Drop                   (* drop *)
  | Swap                   (* swap *)
  | Dup                    (* dup *)
  | Trace                  (* . *)
  | Add                    (* + *)
  | Sub                    (* - *)
  | Mul                    (* * *)
  | Div                    (* / *)
  | Lt                     (* < *)
  | Eq                     (* = *)
  | Bind of ident          (* |> ID *)
  | Call of ident          (* # ID *)
  | If of program          (* ? prog ; *)
  | Def of ident * program (* def prog ; *)
  | Ident of ident         (* ID *)
  | Num of int             (* num *)
and program = command list

let parse_ident = 
  let upper = many1 (satisfy is_upper_case) in
  map (fun x -> implode(x)) upper (* TODO *)

let parse_num =
  let digit = many1 (satisfy is_digit) in
  map (fun x -> implode(x)) digit


(* You are not required to used this but it may be useful in
   understanding how to use `rec_parser` *)
let rec parse_com () =
  let parse_def =
    map2
      (fun id p -> Def (id, p))
      (keyword "def" >> parse_ident << ws)
      (parse_prog_rec () << char ';')
  in
  let parse_bind =
    map
      (fun id-> Bind (id))
      (keyword "|>" >> parse_ident << ws)
  in
  let parse_call =
    map
      (fun id -> Call (id))
      (keyword "#" >> parse_ident << ws)
  in
  let parse_if =
    map
      (fun p -> If (p))
      (keyword "?" >> parse_prog_rec () << char ';')  
  in let p =
    (keyword "drop" >| Drop)
    <|> (keyword "swap" >| Swap)
    <|> (keyword "dup" >| Dup)
    <|> (keyword "." >| Trace)
    <|> (keyword "+" >| Add)
    <|> (keyword "-" >| Sub)
    <|> (keyword "*" >| Mul)
    <|> (keyword "/" >| Div)
    <|> (keyword "<" >| Lt)
    <|> (keyword "=" >| Eq)
    <|> parse_bind
    <|> parse_call
    <|> parse_def
    <|> parse_if
    <|> map (fun id -> Ident id) parse_ident
    <|> map (fun num -> Num (int_of_string num)) parse_num
  in ws >> p
  (* TODO *)


and parse_prog_rec () =
  many ((rec_parser parse_com) << ws)

let parse_prog =
  parse (many (rec_parser parse_com)) (* TODO *)

(* A VERY SMALL TEST SET *)
let test = parse_prog "drop"
let out = Some [Drop]
let _ = assert (test = out)

let test = parse_prog "     .       "
let out = Some [Trace]
let _ = assert (test = out)

let test = parse_prog "  |> TEST   "
let out = Some [Bind "TEST"]
let _ = assert (test = out)

let test = parse_prog "  23 00345 + |> OK "
let out = Some [Num 23; Num 345; Add; Bind "OK"]
let _ = assert (test = out)

let test = parse_prog "  def NEG 0 - ; 2 #NEG 2 =    \n\n   "
let out = Some [Def ("NEG", [Num 0; Sub]); Num 2; Call "NEG"; Num 2; Eq]
let _ = assert (test = out)

let test = parse_prog "
  def ABS
    dup 0 swap < ?
      0 -
    ;
  ;

  30 0 -
  #ABS
  |> X
"
let out = Some
    [ Def ("ABS", [Dup; Num 0; Swap; Lt; If [Num 0; Sub]])
    ; Num 30; Num 0; Sub
    ;  Call "ABS"
    ;  Bind "X"
    ]
let _ = assert (test = out)

(* EVALUATION *)

type stack = int list
type value
  = Num of int
  | Prog of program
type env = (ident * value) list
type trace = string list

let update_env (environment:env) (identifier:ident) (ival:value) : env = 
  (identifier, ival) :: environment
let rec fetch_env (environment:env) (identifier:ident) : value option =
  match environment with
    | [] -> None
    | h::t -> (
      match h with
      | ident, value -> 
        ( if (ident = identifier) then (Some (value)) else (fetch_env (t) (identifier))
    )
  ) (* TODO *)

let rec eval_cmd (conf: stack * env * trace * program) : stack * env * trace * program = 
  match conf with
  | (stack, env, trace, prog) ->
    match prog with
    | [] -> (stack, env, trace, prog)
    | ph::pt ->
      (match ph with
        | Drop -> (
          match stack with
          | sh::st -> eval_cmd (st, env, trace, pt)
          | [] -> (stack, env, ("panic: DropErr")::trace, [])
        )
        | Swap -> (
          match stack with
          | x::y::st -> eval_cmd (y::x::st, env, trace, pt)
          | x::[] -> (stack, env, ("panic: SwapErr1")::trace, [])
          | [] -> (stack, env, ("panic: SwapErr0")::trace, [])
        )
        | Dup -> (
          match stack with
          | sh::st -> eval_cmd (sh::sh::st, env, trace, pt) 
          | [] -> (stack, env, ("panic: DupErr")::trace, [])
        )
        | Trace -> (
          match stack with
          | sh::st -> eval_cmd (st, env, string_of_int(sh)::trace, pt) 
          | [] -> (stack, env, ("panic: TraceErr")::trace, [])
        )
        | Num num -> eval_cmd (num::stack, env, trace, pt)
        | Add -> (
          match stack with
          | x::y::st -> eval_cmd ((x+y)::st, env, trace, pt) 
          | x::[] -> (stack, env, ("panic: AddErr1")::trace, [])
          | [] -> (stack, env, ("panic: AddErr0")::trace, [])
        )
        | Sub -> (
          match stack with
          | x::y::st -> eval_cmd ((x-y)::st, env, trace, pt) 
          | x::[] -> (stack, env, ("panic: SubErr1")::trace, [])
          | [] -> (stack, env, ("panic: SubErr0")::trace, [])
        )
        | Mul -> (
          match stack with
          | x::y::st -> eval_cmd ((x*y)::st, env, trace, pt) 
          | x::[] -> (stack, env, ("panic: MulErr1")::trace, [])
          | [] -> (stack, env, ("panic: MulErr0")::trace, [])
        )
        | Div -> (
          match stack with
          | x::0::st -> (stack, env, ("panic: DivByZero")::trace, [])
          | x::y::st -> eval_cmd ((x/y)::st, env, trace, pt) 
          | x::[] -> (stack, env, ("panic: DivErr1")::trace, [])
          | [] -> (stack, env, ("panic: DivErr0")::trace, [])
        )
        | Lt -> (
          match stack with
          | x::y::st -> (if (x < y) then eval_cmd (1::st, env, trace, pt) else eval_cmd (0::st, env, trace, pt))
          | x::[] -> (stack, env, ("panic: LtErr1")::trace, [])
          | [] -> (stack, env, ("panic: LtErr0")::trace, [])
        )
        | Eq -> (
          match stack with
          | x::y::st -> (if (x = y) then eval_cmd (1::st, env, trace, pt) else eval_cmd (0::st, env, trace, pt))
          | x::[] -> (stack, env, ("panic: EqErr1")::trace, [])
          | [] -> (stack, env, ("panic: EqErr0")::trace, [])
        )
        | Bind ident -> (
          match stack with
          | sh::st -> eval_cmd (st, update_env (env) (ident) (Num sh) , trace, pt)
          | [] -> (stack, env, ("panic: varErr")::trace, [])
        )
        | Ident ident -> (
          match (fetch_env (env) (ident)) with
          | Some (Num num) -> eval_cmd ((num)::stack, env, trace, pt)
          | Some (Prog p) -> (stack, env , ("panic: fetchPErr")::trace, pt)
          | None -> (stack, env, ("panic: fetchErr")::trace, [])
        )
        | Def (ident, p) -> eval_cmd(stack, update_env (env) (ident) (Prog p), trace, pt)
        | Call ident -> (
          match (fetch_env (env) (ident)) with
          | Some (Num num) -> (stack, env , ("panic: callErr")::trace, pt)
          | Some (Prog p) -> eval_cmd (stack, env, trace, p @ pt)
          | None -> (stack, env, ("panic: callErr")::trace, [])
        )
        | If (p) -> (
          match stack with
          | 0::st -> eval_cmd(st, env, trace, pt)
          | x::st -> eval_cmd (st, env, trace, p @ pt)
          | [] -> (stack, env, ("panic: ifErr")::trace, [])
        )
       )
let eval_prog (stk: stack) (en: env) (pr: program)=
  eval_cmd (stk, en, [], pr)  
let interp (input) =
  match parse_prog input with
  | Some (p) -> (match eval_prog [] [] p with
    | (stk, en, trace, program) -> Some (trace)
  )
  | None -> None (* TODO *)

(* END OF PROJECT CODE *)

(* ============================================================ *)

(* UNCOMMENT TO RUN INTERPRETER *)

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

