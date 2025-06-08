open Ast

module StringMap = Map.Make(String)

type value =
  | VInt of int
  | VLong of int64
  | VFloat of float
  | VBool of bool
  | VString of string
  | VFun of string list * expr * env
  | VPrim of (value list -> value)
and env = value StringMap.t

exception RuntimeError of string

let rec eval (env : env) (e : expr) : value =
  match e with
  | Int n -> VInt n
  | Lng n -> VLong n
  | Float f -> VFloat f
  | Bool b -> VBool b
  | Str s -> VString s
  | Add (a, b) ->
      (match eval env a, eval env b with
      | VInt x, VInt y -> VInt (x + y)
      | VLong x, VLong y -> VLong (Int64.add x y)
      | VInt x, VLong y -> VLong (Int64.add (Int64.of_int x) y)
      | VLong x, VInt y -> VLong (Int64.add x (Int64.of_int y))
      | VFloat x, VFloat y -> VFloat (x +. y)
      | VInt x, VFloat y -> VFloat ((float_of_int x) +. y)
      | VFloat x, VInt y -> VFloat (x +. (float_of_int y))
      | _ -> raise (RuntimeError "Type error in add"))
  | Sub (a, b) ->
      (match eval env a, eval env b with
      | VInt x, VInt y -> VInt (x - y)
      | VLong x, VLong y -> VLong (Int64.sub x y)
      | VInt x, VLong y -> VLong (Int64.sub (Int64.of_int x) y)
      | VLong x, VInt y -> VLong (Int64.sub x (Int64.of_int y))
      | VFloat x, VFloat y -> VFloat (x -. y)
      | VInt x, VFloat y -> VFloat ((float_of_int x) -. y)
      | VFloat x, VInt y -> VFloat (x -. (float_of_int y))
      | _ -> raise (RuntimeError "Type error in sub"))
  | Mul (a, b) ->
      (match eval env a, eval env b with
      | VInt x, VInt y -> VInt (x * y)
      | VLong x, VLong y -> VLong (Int64.mul x y)
      | VInt x, VLong y -> VLong (Int64.mul (Int64.of_int x) y)
      | VLong x, VInt y -> VLong (Int64.mul x (Int64.of_int y))
      | VFloat x, VFloat y -> VFloat (x *. y)
      | VInt x, VFloat y -> VFloat ((float_of_int x) *. y)
      | VFloat x, VInt y -> VFloat (x *. (float_of_int y))
      | _ -> raise (RuntimeError "Type error in mul"))
  | Div (a, b) ->
      (match eval env a, eval env b with
       | _, VInt 0 -> raise (RuntimeError "Division by zero")
       | _, VLong 0L -> raise (RuntimeError "Division by zero")
       | VInt x, VInt y ->
         if y = 0 then raise (RuntimeError "Division by zero") else VFloat ((float_of_int x) /. (float_of_int y))
       | VLong x, VLong y ->
         if y = 0L then raise (RuntimeError "Division by zero") else VFloat (Int64.to_float x /. Int64.to_float y)
       | VInt x, VLong y ->
         if y = 0L then raise (RuntimeError "Division by zero") else VFloat ((float_of_int x) /. Int64.to_float y)
       | VLong x, VInt y ->
         if y = 0 then raise (RuntimeError "Division by zero") else VFloat (Int64.to_float x /. (float_of_int y))
       | VFloat x, VFloat y ->
         if y = 0.0 then raise (RuntimeError "Division by zero") else VFloat (x /. y)
       | VInt x, VFloat y ->
         if y = 0.0 then raise (RuntimeError "Division by zero") else VFloat ((float_of_int x) /. y)
       | VFloat x, VInt y ->
         if float_of_int y = 0.0 then raise (RuntimeError "Division by zero") else VFloat (x /. (float_of_int y))
       | _ -> raise (RuntimeError "Type error in div"))
  | Eq (a, b) ->
      let va = eval env a in
      let vb = eval env b in
      let result =
        match va, vb with
        | VInt x, VInt y -> x = y
        | VLong x, VLong y -> x = y
        | VString x, VString y -> x = y
        | VBool x, VBool y -> x = y
        | _ -> false
      in
      VFun (["t"; "f"],
            (if result then Var "t" else Var "f"),
            env)
  | Var x ->
      (try StringMap.find x env
       with Not_found -> raise (RuntimeError ("Unbound variable: " ^ x)))
  | Lam (x, body) -> VFun ([x], body, env)
  | App (f, a) ->
      let vf = eval env f in
      let va = eval env a in
      (match vf with
      | VFun ([], body, closure) ->
          (* Handle zero-parameter functions by evaluating the body directly *)
          eval closure body
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
      | VFun ([], body, closure) ->
          (* Auto-invoke zero-parameter functions when printing
             but don't print the result since the function itself
             might contain print statements *)
          eval closure body
      | _ ->
          print_value v;
          v)

and print_value = function
  | VInt n -> print_endline (string_of_int n)
  | VLong n -> print_endline (Int64.to_string n)
  | VFloat f -> print_endline (string_of_float f)
  | VBool b -> print_endline (string_of_bool b)
  | VString s -> print_endline s
  | VFun ([x], Lam (y, Var vname), _) when vname = x -> print_endline "true"
  | VFun ([x], Lam (y, Var vname), _) when vname = y -> print_endline "false"
  | _ -> print_endline "<fun>"

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
  try
    let env = List.fold_left eval_toplevel StringMap.empty exprs in
    match StringMap.find_opt "main" env with
    | Some (VFun ([], body, closure)) -> ignore (eval closure body)
    | _ -> ()
  with
  | RuntimeError msg ->
      print_endline ("\nLMC Error: " ^ msg);
      (* Don't re-raise the exception, just gracefully terminate *)
      ()
  | Division_by_zero ->
      print_endline "\nLMC Error: Division by zero"
      (* Don't re-raise the exception, just gracefully terminate *)
