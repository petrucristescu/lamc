open Types
open Ast

exception ParseError of string * int * int

type token =
  | Integer of int
  | Long of int64
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
  | Newline
  | Eof

type pos_token = token * int * int

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
    if i >= String.length s then List.rev ((Eof, line, col) :: acc)
    else if s.[i] = '\n' then
      aux (i+1) (line+1) 1 ((Newline, line, col) :: acc)
    else if s.[i] = '(' then
      aux (i+1) line (col+1) ((LParen, line, col) :: acc)
    else if s.[i] = ')' then
      aux (i+1) line (col+1) ((RParen, line, col) :: acc)
    else if s.[i] = '~' then
      aux (i+1) line (col+1) ((Tilde, line, col) :: acc)
    else if s.[i] = ',' then
      aux (i+1) line (col+1) ((Comma, line, col) :: acc)
    else if i + 1 < String.length s && s.[i] = '|' && s.[i+1] = '>' then
      aux (i+2) line (col+2) ((Lambda, line, col) :: acc)
    else if is_whitespace s.[i] then
      aux (i+1) line (col+1) acc
    else if s.[i] = '+' then
      aux (i+1) line (col+1) ((Plus, line, col) :: acc)
    else if s.[i] = '-' then
      aux (i+1) line (col+1) ((Minus, line, col) :: acc)
    else if s.[i] = '.' then
      aux (i+1) line (col+1) ((Dot, line, col) :: acc)
    else if s.[i] = '@' then
      aux (i+1) line (col+1) ((At, line, col) :: acc)
    else if s.[i] = '_' then
      aux (i+1) line (col+1) ((Underscore, line, col) :: acc)
    else if is_letter s.[i] then
      let j = ref (i+1) in
      while !j < String.length s && is_ident_char s.[!j] do
        incr j
      done;
      let name = String.sub s i (!j-i) in
      aux !j line (col + (!j - i)) ((Ident name, line, col) :: acc)
    else if is_digit s.[i] then
      let j = ref i in
      while !j < String.length s && is_digit s.[!j] do incr j done;
      let is_long =
        !j < String.length s && (s.[!j] = 'l' || s.[!j] = 'L')
      in
      if is_long then (
        let n = Int64.of_string (String.sub s i (!j-i)) in
        aux (!j+1) line (col + (!j - i + 1)) ((Long n, line, col) :: acc)
      ) else (
        let n = int_of_string (String.sub s i (!j-i)) in
        aux !j line (col + (!j - i)) ((Integer n, line, col) :: acc)
      )
    else if s.[i] = '"' then
      let j = ref (i+1) in
      while !j < String.length s && s.[!j] <> '"' do incr j done;
      if !j >= String.length s then
        raise (ParseError ("Unterminated string literal", line, col))
      else
        let str = String.sub s (i+1) (!j - i - 1) in
        aux (!j+1) line (col + (!j - i + 1)) ((String str, line, col) :: acc)
    else
      raise (ParseError ("Unknown char: " ^ String.make 1 s.[i], line, col))
  in
  aux 0 1 1 []

let rec parse_expr tokens = parse_fun_def tokens

and skip_newlines = function
  | (Newline, _, _) :: rest -> skip_newlines rest
  | tokens -> tokens

and parse_seq tokens =
  let rec aux toks acc =
    let toks = skip_newlines toks in
    match toks with
    | (RParen, _, _) :: rest -> (List.rev acc, rest)
    | [] | (Eof, _, _) :: _ -> (List.rev acc, toks)
    | _ ->
        let expr, toks' = parse_expr toks in
        aux toks' (expr :: acc)
  in
  let exprs, rest = aux tokens [] in
  match exprs with
  | [] -> (Int 0, rest)
  | [e] -> (e, rest)
  | e1 :: es -> (List.fold_left (fun acc e -> Seq (acc, e)) e1 es, rest)

and parse_fun_def tokens =
  match tokens with
  | (Tilde, _, _) :: (LParen, _, _) :: rest ->
      let body, rest' = parse_seq rest in
      (FunDef ("main", [], body), rest')
  | (Tilde, _, _) :: (Ident name, _, _) :: rest ->
      let rec parse_args toks acc =
        match toks with
        | (LParen, _, _) :: _ -> (List.rev acc, toks)
        | (Ident arg, _, _) :: (Comma, _, _) :: rest' -> parse_args rest' (arg :: acc)
        | (Ident arg, _, _) :: rest' -> (List.rev (arg :: acc), rest')
        | (tok, line, col) :: _ -> raise (ParseError ("Expected argument name", line, col))
        | [] -> raise (ParseError ("Expected argument name", 1, 1))
      in
      let args, rest' = parse_args rest [] in
      let body, rest'' =
        match rest' with
        | (LParen, _, _) :: rest'' -> parse_seq rest''
        | _ -> parse_add rest'
      in
      (FunDef (name, args, body), rest'')
  | _ -> parse_var_def tokens

and parse_var_def tokens =
  let parse_typed_var name value rest line col =
    match String.split_on_char '_' name with
    | [t; v] ->
        let typ = match t with
          | "i" -> TInt
          | "l" -> TLong
          | "s" -> TString
          | _ -> TUnknown
        in
        (Let (v, typ, value), rest)
    | _ -> (Let (name, TUnknown, value), rest)
  in
  match tokens with
  | (At, line, col) :: (Ident name, _, _) :: (Integer n, _, _) :: rest ->
      parse_typed_var name (Int n) rest line col
  | (At, line, col) :: (Ident name, _, _) :: (Long n, _, _) :: rest ->
      parse_typed_var name (Lng n) rest line col
  | (At, line, col) :: (Ident name, _, _) :: (String s, _, _) :: rest ->
      parse_typed_var name (Str s) rest line col
  | (At, line, col) :: (Ident name, _, _) :: rest ->
      (match rest with
       | (Integer n, _, _) :: rest' -> parse_typed_var name (Int n) rest' line col
       | (String s, _, _) :: rest' -> parse_typed_var name (Str s) rest' line col
       | (tok, l, c) :: _ -> raise (ParseError ("Expected value after variable name", l, c))
       | [] -> raise (ParseError ("Expected value after variable name", line, col)))
  | _ -> parse_add tokens

and parse_add tokens =
  let lhs, rest = parse_app tokens in
  match rest with
  | (Plus, _, _) :: rest' ->
      let rhs, rest'' = parse_add rest' in
      (Add (lhs, rhs), rest'')
  | (Minus, _, _) :: rest' ->
      let rhs, rest'' = parse_add rest' in
      (Sub (lhs, rhs), rest'')
  | _ -> (lhs, rest)

and parse_app tokens =
  let rec aux acc toks =
    match toks with
    | (Integer _, _, _) :: _
    | (Long _, _, _) :: _
    | (Ident _, _, _) :: _
    | (LParen, _, _) :: _
    | (String _, _, _) :: _ ->
        let arg, rest = parse_atom toks in
        aux (App (acc, arg)) rest
    | _ -> (acc, toks)
  in
  let atom, rest = parse_atom tokens in
  aux atom rest

and parse_atom tokens =
  match tokens with
  | (Integer n, _, _) :: rest -> (Int n, rest)
  | (Long n, _, _) :: rest -> (Lng n, rest)
  | (Ident "print", _, _) :: rest ->
      let arg, rest' = parse_expr rest in
      (Print arg, rest')
  | (Ident x, _, _) :: rest -> (Var x, rest)
  | (LParen, _, _) :: rest ->
      let expr, rest' = parse_expr rest in
      (match rest' with
       | (RParen, _, _) :: rest'' -> (expr, rest'')
       | (tok, line, col) :: _ -> raise (ParseError ("Expected closing parenthesis", line, col))
       | [] -> raise (ParseError ("Expected closing parenthesis", 1, 1)))
  | (String s, _, _) :: rest -> (Str s, rest)
  | (At, _, _) :: _ -> parse_var_def tokens
  | (tok, line, col) :: _ -> raise (ParseError ("Invalid expression", line, col))
  | [] -> raise (ParseError ("Invalid expression", 1, 1))

let rec parse_all tokens acc =
  let tokens = skip_newlines tokens in
  match tokens with
  | [] | (Eof, _, _) :: _ -> List.rev acc
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

(* --- Type inference integration --- *)
let string_of_typ t =
  let rec aux = function
    | TInt -> "Int"
    | TLong -> "Long"
    | TString -> "String"
    | TVar v -> "'" ^ v
    | TFun (a, b) -> "(" ^ aux a ^ " -> " ^ aux b ^ ")"
    | TUnknown -> "?"
  in aux t

let parse_and_infer ?(show_types=true) input =
  let exprs = parse input in
  let env = Types.StringMap.empty in
  let rec infer_toplevel env = function
    | [] -> ()
    | expr :: rest ->
        try
          let _, typ = Infer.infer env expr in
          if show_types then
            Printf.printf "Expr: %s\nType: %s\n\n" (Ast.string_of_expr expr) (string_of_typ typ);
          let env' =
            match expr with
            | Let (name, _, _) -> Types.StringMap.add name typ env
            | FunDef (name, _, _) -> Types.StringMap.add name typ env
            | _ -> env
          in
          infer_toplevel env' rest
        with
        | Infer.TypeError msg -> Printf.printf "Type error: %s\n" msg; infer_toplevel env rest
  in
  infer_toplevel env exprs;
  exprs