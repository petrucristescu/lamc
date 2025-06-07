exception EvalError of string

type value =
  | IntVal of int
  | StrVal of string
  | FunVal of string list * Parser.expr * env
and env = (string * value) list

let rec eval (env : env) (e : Parser.expr) : value =
  match e with
  | Parser.Int n -> IntVal n
  | Parser.Str s -> StrVal s
  | Parser.Add (a, b) ->
      (match eval env a, eval env b with
       | IntVal x, IntVal y -> IntVal (x + y)
       | _ -> raise (EvalError "Type error in addition"))
  | Parser.Sub (a, b) ->
      (match eval env a, eval env b with
       | IntVal x, IntVal y -> IntVal (x - y)
       | _ -> raise (EvalError "Type error in subtraction"))
  | Parser.Var x ->
      (try List.assoc x env with Not_found -> raise (EvalError ("Undefined variable: " ^ x)))
  | Parser.Lam (x, body) -> FunVal ([x], body, env)
  | Parser.App (f, arg) ->
      let fv = eval env f in
      let av = eval env arg in
      (match fv with
       | FunVal ([x], body, closure) -> eval ((x, av) :: closure) body
       | FunVal (args, body, closure) ->
           (match args with
            | x :: xs ->
                let new_env = (x, av) :: closure in
                if xs = [] then eval new_env body
                else FunVal (xs, body, new_env)
            | [] -> raise (EvalError "Function with no arguments"))
       | _ -> raise (EvalError "Not a function"))
  | Parser.Let (name, _, value) ->
      let v = eval env value in
      v
  | Parser.FunDef (name, args, body) ->
      FunVal (args, body, env)
  | Print e ->
      let v = eval env e in
      (match v with
       | IntVal n -> print_endline (string_of_int n)
       | StrVal s -> print_endline s
       | FunVal _ -> print_endline "<function>");
      v

let rec eval_toplevel env = function
  | [] -> ()
  | Parser.Let (name, _, value) :: rest ->
      let v = eval env value in
      eval_toplevel ((name, v) :: env) rest
  | Parser.FunDef (name, args, body) :: rest ->
      let v = FunVal (args, body, env) in
      eval_toplevel ((name, v) :: env) rest
  | expr :: rest ->
      (try
         let _ = eval env expr in
         eval_toplevel env rest
       with
       | EvalError msg -> Printf.printf "Evaluation error: %s\n" msg)