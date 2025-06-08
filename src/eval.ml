open Ast

module StringMap = Map.Make(String)

type value =
  | VInt of int
  | VLong of int64
  | VString of string
  | VFun of string list * expr * env
  | VPrim of (value list -> value)
and env = value StringMap.t

exception RuntimeError of string

let rec eval (env : env) (e : expr) : value =
  match e with
  | Int n -> VInt n
  | Lng n -> VLong n
  | Str s -> VString s
  | Add (a, b) ->
      (match eval env a, eval env b with
      | VInt x, VInt y -> VInt (x + y)
      | VLong x, VLong y -> VLong (Int64.add x y)
      | VInt x, VLong y -> VLong (Int64.add (Int64.of_int x) y)
      | VLong x, VInt y -> VLong (Int64.add x (Int64.of_int y))
      | _ -> raise (RuntimeError "Type error in add"))
  | Sub (a, b) ->
      (match eval env a, eval env b with
      | VInt x, VInt y -> VInt (x - y)
      | VLong x, VLong y -> VLong (Int64.sub x y)
      | VInt x, VLong y -> VLong (Int64.sub (Int64.of_int x) y)
      | VLong x, VInt y -> VLong (Int64.sub x (Int64.of_int y))
      | _ -> raise (RuntimeError "Type error in sub"))
  | Var x ->
      (try StringMap.find x env
       with Not_found -> raise (RuntimeError ("Unbound variable: " ^ x)))
  | Lam (x, body) -> VFun ([x], body, env)
  | App (f, a) ->
      let vf = eval env f in
      let va = eval env a in
      (match vf with
      | VFun (x::xs, body, closure) ->
          let env' = StringMap.add x va closure in
          if xs = [] then eval env' body
          else VFun (xs, body, env')
      | VPrim fn -> fn [va]
      | _ -> raise (RuntimeError "Attempt to call a non-function"))
  | Let (name, _, value) ->
      let v = eval env value in
      VFun ([], Var name, StringMap.add name v env) (* Not used at top-level *)
  | FunDef (name, args, body) ->
      let f = VFun (args, body, env) in
      VFun ([], Var name, StringMap.add name f env) (* Not used at top-level *)
  | Seq (a, b) ->
      ignore (eval env a);
      eval env b
  | Print e ->
      let v = eval env e in
      (match v with
      | VInt n -> print_endline (string_of_int n)
      | VLong n -> print_endline (Int64.to_string n)
      | VString s -> print_endline s
      | _ -> print_endline "<fun>");
      v

let rec eval_toplevel (env : env) (expr : expr) : env =
  match expr with
  | Seq (a, b) ->
      let env' = eval_toplevel env a in
      eval_toplevel env' b
  | Let (name, _, value) ->
      let v = eval env value in
      StringMap.add name v env
  | FunDef (name, args, body) ->
      let f = VFun (args, body, env) in
      StringMap.add name f env
  | _ ->
      ignore (eval env expr);
      env

let eval_program exprs =
  let env = List.fold_left eval_toplevel StringMap.empty exprs in
  match StringMap.find_opt "main" env with
  | Some (VFun ([], body, closure)) -> ignore (eval closure body)
  | _ -> ()