exception ParseError of string * int * int  (* message, line, col *)

type token =
  | Number of int
  | String of string
  | Plus
  | Minus
  | Lambda
  | Ident of string
  | At
  | Underscore
  | TypeName of string
  | Dot
  | Tilde
  | LParen
  | RParen
  | Comma
  | Eof

type typ =
  | TInt
  | TString
  | TUnknown

type expr =
  | Int of int
  | Str of string
  | Add of expr * expr
  | Sub of expr * expr
  | Var of string
  | Lam of string * expr
  | App of expr * expr
  | Let of string * typ * expr  (* variable name, type, value *)
  | FunDef of string * string list * expr
  | Print of expr

let tokenize s =
  let is_letter c =
    ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c = '_'
  in
  let is_ident_char c =
    is_letter c || ('0' <= c && c <= '9')
  in
  let is_digit c =
    '0' <= c && c <= '9'
  in
  let is_whitespace c =
    c = ' ' || c = '\t' || c = '\r'
  in
  let rec aux i line col acc =
    if i >= String.length s then List.rev (Eof :: acc)
    else if s.[i] = '(' then
      aux (i+1) line (col+1) (LParen :: acc)
    else if s.[i] = ')' then
      aux (i+1) line (col+1) (RParen :: acc)
    else if s.[i] = '~' then
      aux (i+1) line (col+1) (Tilde :: acc)
    else if s.[i] = ',' then
      aux (i+1) line (col+1) (Comma :: acc)
    else if i + 1 < String.length s && s.[i] = '|' && s.[i+1] = '>' then
      aux (i+2) line (col+2) (Lambda :: acc)
    else if is_whitespace s.[i] then
      aux (i+1) line (col+1) acc
    else if s.[i] = '\n' then
      aux (i+1) (line+1) 1 acc
    else if s.[i] = '+' then
      aux (i+1) line (col+1) (Plus :: acc)
    else if s.[i] = '-' then
      aux (i+1) line (col+1) (Minus :: acc)
    else if s.[i] = '.' then
      aux (i+1) line (col+1) (Dot :: acc)
    else if s.[i] = '@' then
      aux (i+1) line (col+1) (At :: acc)
    else if s.[i] = '_' then
      aux (i+1) line (col+1) (Underscore :: acc)
    else if is_letter s.[i] then
      let j = ref (i+1) in
      while !j < String.length s && is_ident_char s.[!j] do incr j done;
      let name = String.sub s i (!j-i) in
      aux !j line (col + (!j - i)) (Ident name :: acc)
    else if is_digit s.[i] then
      let j = ref i in
      while !j < String.length s && is_digit s.[!j] do incr j done;
      let n = int_of_string (String.sub s i (!j-i)) in
      aux !j line (col + (!j - i)) (Number n :: acc)
    else if s.[i] = '"' then
      let j = ref (i+1) in
      while !j < String.length s && s.[!j] <> '"' do incr j done;
      if !j >= String.length s then
        raise (ParseError ("Unterminated string literal", line, col))
      else
        let str = String.sub s (i+1) (!j - i - 1) in
        aux (!j+1) line (col + (!j - i + 1)) (String str :: acc)
    else
      raise (ParseError ("Unknown char: " ^ String.make 1 s.[i], line, col))
  in aux 0 1 1 []

let rec parse_expr tokens = parse_fun_def tokens

and parse_fun_def tokens =
  match tokens with
  | Tilde :: Ident name :: rest ->
      let rec parse_args toks acc =
        match toks with
        | Ident arg :: Comma :: rest' -> parse_args rest' (arg :: acc)
        | Ident arg :: rest' -> (List.rev (arg :: acc), rest')
        | _ -> raise (ParseError ("Expected argument name", 1, 1))
      in
      let args, rest' = parse_args rest [] in
      let body, rest'' = parse_add rest' in
      (FunDef (name, args, body), rest'')
  | _ -> parse_var_def tokens

and parse_var_def tokens =
  let parse_typed_var name value rest =
    match String.split_on_char '_' name with
    | [t; v] ->
        let typ = match t with
          | "i" -> TInt
          | "s" -> TString
          | _ -> TUnknown
        in
        (Let (v, typ, value), rest)
    | _ -> (Let (name, TUnknown, value), rest)
  in
  match tokens with
  | At :: Ident name :: Number n :: rest ->
      parse_typed_var name (Int n) rest
  | At :: Ident name :: String s :: rest ->
      parse_typed_var name (Str s) rest
  | At :: Ident name :: rest ->
      (match rest with
       | Number n :: rest' -> parse_typed_var name (Int n) rest'
       | String s :: rest' -> parse_typed_var name (Str s) rest'
       | _ -> raise (ParseError ("Expected value after variable name", 1, 1)))
  | _ -> parse_add tokens

and parse_add tokens =
  let lhs, rest = parse_app tokens in
  match rest with
  | Plus :: rest' ->
      let rhs, rest'' = parse_add rest' in
      (Add (lhs, rhs), rest'')
  | Minus :: rest' ->
      let rhs, rest'' = parse_add rest' in
      (Sub (lhs, rhs), rest'')
  | _ -> (lhs, rest)

and parse_app tokens =
  match tokens with
  | LParen :: Ident f :: rest ->
      let rec parse_args toks acc =
        match toks with
        | RParen :: rest' -> (List.rev acc, rest')
        | _ ->
            let arg, toks' = parse_expr toks in
            parse_args toks' (arg :: acc)
      in
      let args, rest' = parse_args rest [] in
      let app = List.fold_left (fun acc arg -> App (acc, arg)) (Var f) args in
      (app, rest')
  | Lambda :: Ident x :: Dot :: rest ->
      let body, rest' = parse_expr rest in
      (Lam (x, body), rest')
  | _ ->
      parse_atom tokens

and parse_atom tokens =
  match tokens with
  | Number n :: rest -> (Int n, rest)
  | Ident "print" :: rest ->
      let arg, rest' = parse_expr rest in
      (Print arg, rest')
  | Ident x :: rest -> (Var x, rest)
  | LParen :: rest ->
      let expr, rest' = parse_expr rest in
      (match rest' with
       | RParen :: rest'' -> (expr, rest'')
       | _ -> raise (ParseError ("Expected closing parenthesis", 1, 1)))
  | String s :: rest -> (Str s, rest)
  | _ -> raise (ParseError ("Invalid expression", 1, 1))

let rec string_of_expr = function
  | Int n -> string_of_int n
  | Add (a, b) -> "(" ^ string_of_expr a ^ " + " ^ string_of_expr b ^ ")"
  | Sub (a, b) -> "(" ^ string_of_expr a ^ " - " ^ string_of_expr b ^ ")"
  | Var x -> x
  | Lam (x, body) -> "|>" ^ x ^ ". " ^ string_of_expr body
  | App (f, a) -> "(" ^ string_of_expr f ^ " " ^ string_of_expr a ^ ")"
  | Str s -> "\"" ^ s ^ "\""
  | Let (name, typ, value) ->
      "@" ^ name ^ " : " ^
      (match typ with TInt -> "Int" | TString -> "String" | TUnknown -> "?") ^
      " = " ^ string_of_expr value
  | FunDef (name, args, body) ->
      "~" ^ name ^ " " ^ String.concat "," args ^ " " ^ string_of_expr body
  | Print e -> "print " ^ string_of_expr e

let string_of_exprs exprs =
  String.concat "\n" (List.map string_of_expr exprs)

let rec parse_all tokens acc =
  match tokens with
  | [] | [Eof] -> List.rev acc
  | Eof :: _ -> List.rev acc
  | _ ->
      let expr, rest = parse_expr tokens in
      parse_all rest (expr :: acc)

let parse input =
  let tokens =
    try tokenize input
    with ParseError (msg, line, col) ->
      Printf.printf "Tokenization error at line %d, col %d: %s\n" line col msg;
      raise (ParseError (msg, line, col))
  in
  try parse_all tokens []
  with ParseError (msg, line, col) ->
    raise (ParseError (msg, line, col))