open Parser

exception EvalError of string

type value =
  | IntVal of int
  | StrVal of string
  | FunVal of string list * expr * env
and env = (string * value) list

let rec eval env = function
  | Int n -> IntVal n
  | Str s -> StrVal s
  | Add (a, b) ->
      (match eval env a, eval env b with
       | IntVal x, IntVal y -> IntVal (x + y)
       | _ -> failwith "Type error in addition")
  | Sub (a, b) ->
      (match eval env a, eval env b with
       | IntVal x, IntVal y -> IntVal (x - y)
       | _ -> failwith "Type error in subtraction")
  | Var x ->
      (try List.assoc x env with Not_found -> failwith ("Unbound variable: " ^ x))
  | Lam (x, body) -> FunVal ([x], body, env)
  | App (f, a) ->
      (match eval env f with
       | FunVal (params, body, closure_env) when List.length params = 1 ->
           let arg_val = eval env a in
           let new_env = (List.hd params, arg_val) :: closure_env in
           eval new_env body
       | FunVal (param :: rest_params, body, closure_env) ->
           let arg_val = eval env a in
           let new_env = (param, arg_val) :: closure_env in
           if rest_params = [] then
             eval new_env body
           else
             FunVal (rest_params, body, new_env)
       | _ -> failwith "Attempt to call a non-function")
  | Let (name, _, value) ->
      let v = eval env value in
      v
  | FunDef (name, args, body) ->
      FunVal (args, body, env)
  | Seq (e1, e2) ->
      ignore (eval env e1);
      eval env e2
  | Print e ->
      let v = eval env e in
      (match v with
       | IntVal n -> print_endline (string_of_int n)
       | StrVal s -> print_endline s
       | FunVal _ -> print_endline "<function>");
      v

let rec eval_toplevel env = function
  | [] -> ()
  | Let (name, _, value) :: rest ->
      let v = eval env value in
      Printf.printf "Let %s = %s\n"
        name
        (match v with
         | IntVal n -> string_of_int n
         | StrVal s -> s
         | FunVal _ -> "<function>");
      eval_toplevel ((name, v) :: env) rest
  | FunDef (name, args, body) :: rest ->
      if name = "main" && args = [] then (
        let _ = eval env body in
        eval_toplevel env rest
      ) else (
        let v = FunVal (args, body, env) in
        Printf.printf "Fun %s = <function>\n" name;
        eval_toplevel ((name, v) :: env) rest
      )
  | _ :: rest ->
      (* Skip any other top-level expressions *)
      eval_toplevel env rest