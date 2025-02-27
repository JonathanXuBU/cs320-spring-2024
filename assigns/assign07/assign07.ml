(* BEGIN UTILITY FUNCTIONS *)

let is_lower_case c =
  'a' <= c && c <= 'z'

let is_upper_case c =
  'A' <= c && c <= 'Z'

let is_alpha c =
  is_lower_case c || is_upper_case c

let is_digit c =
  '0' <= c && c <= '9'

let is_alphanum c =
  is_lower_case c ||
  is_upper_case c ||
  is_digit c

let is_blank c =
  String.contains " \012\n\r\t" c

let explode s =
  List.of_seq (String.to_seq s)

let implode ls =
  String.of_seq (List.to_seq ls)

(* `span p l = (ft, bk)` where `ft` is the longest prefix of `l` such
   that `p x` is true for every element of `ft` and `ft @ bk = l`.
*)
let span (p : 'a -> bool) (l : 'a list) : 'a list * 'a list =
  let rec go acc r =
    match r with
    | [] -> l, []
    | x :: xs ->
      if p x
      then go (x :: acc) xs
      else List.rev acc, r
  in go [] l

(* END UTILITY FUNCTIONS *)

(* ============================================================ *)

(* Homework 7 : Parsing BNF Specifications

   In this assignment, you will be implementing an ad hoc parser for
   BNF specifications (without alternatives).

   A terminal symbol is represented as a nonempty contiguous sequence
   of lowercase letters.

   Examples: "abc" "aaaa" "x" "example"

   A nonterminal symbols is represented by a nonempty contiguous
   sequence of lowercase letters surrounded by '<' and '>'.  In
   particular, there can be no whitespace between the brackets.

   Examples: "<abc>" "<aaaa>" "<x>" "<example>"

   A rule is represented by a nonterminal symbol, followed by the
   symbol '::=', followed by a sequence of symbols, and ending in '.'
   a period symbol.  Rules are whitespace agnostic except that
   consecutive terminal symbols are must be separated by at least one
   space.

   Examples:
   "<abc> ::= <aaaa><x><example>."
   "<abc> ::= <aaaa>   <x>   <example>    .      "
   "<a>::=a  asdf  asdf  <x><y>."

   A grammar is represented by a sequences of rules.

   Example:
   "<abc> ::= <aaaa><x><example>.
    <abc> ::= <aaaa>   <x>   <example>.
    <a>::=a  asdf  asdf  <x><y>."
*)

(* ============================================================ *)

(* Problem 1: Tokenizing

   Implement the function `next_token` which, given

     cs : char list

   returns the token for the first lexeme in `cs`, ignoring leading
   whitespace, along with the remainder of the characters after the
   end of the lexeme.

   The EOFT token should be returned in the case `next_token` is
   called on the empty list.

   Hint: The function `span` above may be useful.

*)

type token
  =
  | EqT             (* ::= *)
  | NtmT of string  (* <id> *)
  | TmT of string   (* id *)
  | PdT             (* . *)
  | EOFT            (* end of file *)

let next_token (cs : char list) : (token * char list) option =
  let rec go (cs : char list) : (token * char list) option =
    match span is_blank cs with
    | [], tk -> (
      match tk with
      | ':'::':'::'='::rest -> Some (EqT, rest)
      | '<' :: rest -> (
        match span is_lower_case rest with
        | nterm , rest1 -> (
          match rest1 with
          | '>' :: rest2 -> Some (NtmT (implode(nterm)), rest2)
          | _ -> None
        )
      )
      | '.' :: rest -> Some (PdT, rest)
      | char :: rest -> (
        match span is_lower_case tk with
        | chrs, rest1 -> Some (TmT (implode(chrs)), rest1)
      )
      | _ -> None
    )
    | _, [] -> Some (EOFT, [])
    | _, nonwhite -> go nonwhite
  in go cs

let tokenize (s : string) : (token list) option =
  let rec go cs =
    match next_token cs with
    | None -> None
    | Some (EOFT, _) -> Some []
    | Some (t, []) -> Some [t]
    | Some (t, rest) ->
      match go rest with
      | None -> None
      | Some ts -> Some (t :: ts)
  in go (explode s)


let _ = assert(next_token (explode "\n ::= q[qpo;laksjd") = Some (EqT, explode " q[qpo;laksjd"))
let _ = assert(next_token (explode "<asdf>   ...") = Some (NtmT "asdf", explode "   ..."))
let _ = assert(next_token (explode "   term  term ") = Some (TmT "term", explode "  term "))
let _ = assert(next_token (explode "...") = Some (PdT, explode ".."))
let _ = assert(next_token (explode " \n \t \r   ") = Some (EOFT, []))
let _ = assert(next_token (explode "<not-good>") = None)

let _ = assert(tokenize "..::=" = Some [PdT;PdT;EqT])
let _ = assert(tokenize "<a> ::= aab a<b>a." = Some [NtmT "a"; EqT; TmT "aab"; TmT "a"; NtmT "b"; TmT "a"; PdT])
let _ = assert(tokenize "<a> ::= aab a<no-good>a." = None)


(* END OF PROBLEM 1 *)

(* ============================================================ *)

(* Problem 2: ADTs

   A symbol is abstractly represented by an ADT with variants `NT` and
   `T`, each carrying a string representing the identifier for the
   symbol.

   A sentential form is represented as a list of symbols.

   A rule is represented as a string (the identifier of a nonterminal
   symbol) and a sentential form where, for example

     ("id", [T "a"; NT "b"; T "c"])

   represents the rule

     <id> ::= a <b> c .

   A grammar is represented as a list of rules.

   ============================================================

   Implement the function `expand_leftmost` which, given

     (nt, sf) : rule
     s : sent_form

   expands the leftmost occurrence of the nonterminal symbol `<nt>` in
   `s` (if it exists) to the sentential form `sf`.

*)

type symbol
  = NT of string
  | T of string

type sentform = symbol list
type rule = string * sentform
type grammar = rule list

let rec expand_leftmost ((nt, sf) : rule) (s : sentform) : sentform =
  match s with
  | h :: rest -> (
    match h with
    | NT nt -> sf @ rest
    | _ -> [h] @ expand_leftmost (nt, sf) rest
  )
  | [] -> []

(* <a> ::= a<a>. *)
let r = "a", [T "a"; NT "a"]

(*
(* <a> --> a<a> *)
let _ = assert (expand_leftmost r [NT "a"] = [T "a"; NT "a"])
(* <a> --> a<a> --> aa<a> *)
let _ = assert (expand_leftmost r (expand_leftmost r [NT "a"]) = [T "a"; T "a"; NT "a"])
(* <a>b<a> --> a<a>b<a> *)
let _ = assert (expand_leftmost r [NT "a"; T "b"; NT "a"] = [T "a"; NT "a"; T "b"; NT "a"])
*)

(* END OF PROBLEM 2 *)

(* ============================================================ *)

(* Problem 3: Parsing BNF Specifications

   Implement the following functions for parsing a BNF specification,
   as described above.  This is, in essence, an ad hoc
   recursive-descent parser.

   `parse_sent_form` gets the LONGEST NONEMPTY sentential form from
   the beginning of a list of tokens, also returning the remaining
   tokens.  Note that we return `None` in the case that the list of
   tokens does not start with a symbol.

   `parse_rule` gets a single rule from the beginning of a list of
   tokens, also returning the remaining tokens.

   `parse_grammar` gets the LONGEST sequence of rules from the
   beginning of a list of tokens, also returning the remaining tokens.
   Note that there is no need to return an option because the list of
   rules can be empty.  The order or rules should be maintained.

*)

let parse_sentform (ts : token list) : (sentform * token list) option =
  let rec go (tok : token list) (s :sentform): (sentform * token list) =
    match tok with
    | h :: rest -> (
      match h with
      | NtmT a -> go rest (s@[NT a])
      | TmT a -> go rest (s@[T a])
      | _ -> (s, tok)
    )
    | [] -> (s, [])
  in
  match ts with 
  | NtmT a :: t -> Some (go ts [])
  | TmT a :: t -> Some (go ts [])
  | _ -> None

let parse_rule (ts : token list) : (rule * token list) option =
  match ts with
  | NtmT c :: EqT :: rest -> (
    match parse_sentform rest with
    | Some ans ->
      (
        match ans with
        | sform, PdT :: tail -> Some ((c, sform), tail)
        | _ -> None
      )
    | None -> None
  )
  | _ -> None

let rec parse_grammar (ts : token list) : grammar * token list =
  let rec go (tok :token list) (acc : grammar) : grammar * token list =
    match parse_rule tok with
    | Some (rule, rest) -> go rest (acc@[rule])
    | None -> acc , tok
  in go ts []

let parse_and_check (s : string) : grammar option =
  match tokenize s with
  | None -> None
  | Some ts ->
    let (g, rest) = parse_grammar ts in
    if List.is_empty rest
    then Some g
    else None

(*
let _ = assert (parse_sentform [NtmT "a"; TmT "b"; NtmT "a"; PdT; PdT; PdT] = Some ([NT "a"; T "b"; NT "a"], [PdT; PdT; PdT]))
let _ = assert (parse_sentform [PdT; PdT; PdT] = None)
let _ = assert (parse_rule [NtmT "a"; EqT; TmT "a"; NtmT "a"; PdT; PdT; PdT] = Some (("a", [T "a"; NT "a"]), [PdT; PdT]))
let _ = assert (parse_rule [NtmT "a"; EqT; TmT "a"; NtmT "a"; NtmT "a"; EqT; NtmT "a"] = None)
*)

let simple_test = "
  <a> ::= a <a> a b .
  <a> ::= <b> .
  <b> ::= b <b> .
  <b> ::= <c> .
  <c> ::= d .
  <c> ::= e f .
  <c> ::= g .
"

let simple_test_out =
  [ "a", [T "a"; NT "a"; T "a"; T "b"]
  ; "a", [NT "b"]
  ; "b", [T "b"; NT "b"]
  ; "b", [NT "c"]
  ; "c", [T "d"]
  ; "c", [T "e"; T "f"]
  ; "c", [T "g"]
  ]

let simple_test_missing_period = "
  <a> ::= a <a> a b .
  <a> ::= <b>
  <b> ::= b <b> .
  <b> ::= <c> .
  <c> ::= d .
  <c> ::= e f .
  <c> ::= g .
"
(*
let _ = assert (parse_and_check simple_test = Some simple_test_out)
let _ = assert (parse_and_check simple_test_missing_period = None)
*)
(* END OF PROBLEM 3 *)
