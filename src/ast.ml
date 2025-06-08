type expr =
  | Int of int
  | Lng of int64
  | Bool of bool
  | Str of string
  | Add of expr * expr
  | Sub of expr * expr
  | Var of string
  | Lam of string * expr
  | App of expr * expr
  | Let of string * Types.typ * expr
  | FunDef of string * string list * expr
  | Seq of expr * expr
  | Print of expr

let rec string_of_typ = function
  | Types.TInt -> "Int"
  | Types.TBool -> "Bool"
  | Types.TString -> "String"
  | Types.TLong -> "Long"
  | Types.TUnknown -> "?"
  | Types.TVar v -> "'" ^ v
  | Types.TFun (a, b) -> "(" ^ string_of_typ a ^ " -> " ^ string_of_typ b ^ ")"

let rec string_of_expr = function
  | Int n -> string_of_int n
  | Lng n -> Int64.to_string n
  | Bool b -> string_of_bool b
  | Add (a, b) -> "(" ^ string_of_expr a ^ " + " ^ string_of_expr b ^ ")"
  | Sub (a, b) -> "(" ^ string_of_expr a ^ " - " ^ string_of_expr b ^ ")"
  | Var x -> x
  | Lam (x, body) -> "|>" ^ x ^ ". " ^ string_of_expr body
  | App (f, a) -> "(" ^ string_of_expr f ^ " " ^ string_of_expr a ^ ")"
  | Str s -> "\"" ^ s ^ "\""
  | Let (name, typ, value) ->
      "@" ^ name ^ " : " ^ string_of_typ typ ^ " = " ^ string_of_expr value
  | FunDef (name, args, body) ->
      "~" ^ name ^ " " ^ String.concat "," args ^ " " ^ string_of_expr body
  | Seq (a, b) -> string_of_expr a ^ ";\n" ^ string_of_expr b
  | Print e -> "print " ^ string_of_expr e

let string_of_exprs exprs =
  String.concat "\n" (List.map string_of_expr exprs)

let print_ast exprs =
  print_endline (string_of_exprs exprs)