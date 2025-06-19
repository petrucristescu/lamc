open Ast

module StringMap = Map.Make(String)

type value =
  | VInt of int
  | VLong of int64
  | VFloat of float
  | VString of string
  | VFun of string list * expr * env
  | VPrim of (value list -> value)
and env = value StringMap.t

exception RuntimeError of string

(* Load and parse a library file *)
let load_library library_name =
  (* Define multiple possible library paths to try *)
  let possible_paths = [
    (* Path relative to executable in _build *)
    Filename.concat (Filename.dirname Sys.argv.(0)) "../lib";
    (* Path in source directory *)
    Filename.concat (Filename.dirname Sys.argv.(0)) "../../src/lib";
    (* Absolute path to src/lib - use this as fallback *)
    Sys.getcwd() ^ "/src/lib"
  ] in

  (* Try to find the library file in any of the possible paths *)
  let rec find_library paths =
    match paths with
    | [] -> raise (RuntimeError ("Library not found: " ^ library_name))
    | path :: rest ->
        let lib_file = Filename.concat path (library_name ^ ".lmc") in
        if Sys.file_exists lib_file then
          lib_file
        else
          find_library rest
  in

  let lib_file = find_library possible_paths in
  try
    let ic = open_in lib_file in
    let len = in_channel_length ic in
    let src = really_input_string ic len in
    close_in ic;
    let exprs = Parser.parse src in
    exprs
  with
  | Sys_error msg -> raise (RuntimeError ("Library error: " ^ msg))
  | Parser.ParseError (msg, line, col) ->
      raise (RuntimeError (Printf.sprintf "Parse error in library %s at line %d, col %d: %s"
                           library_name line col msg))

(* Import cache to avoid loading the same library multiple times *)
let imported_libraries = ref StringMap.empty

(* Print function for values *)
let print_value = function
  | VInt n -> print_endline (string_of_int n)
  | VLong n -> print_endline (Int64.to_string n)
  | VFloat f ->
      if Float.equal (Float.round f) f then
        (* If the float is a whole number, print it as an integer without the decimal point *)
        print_endline (string_of_int (int_of_float f))
      else
        print_endline (string_of_float f)
  | VString s -> print_endline s
  | VFun ([x], Lam (y, Var vname), _) when vname = x -> print_endline "true"
  | VFun ([x], Lam (y, Var vname), _) when vname = y -> print_endline "false"
  | _ -> print_endline "<fun>"

(* Special handling for Church-encoded boolean functions *)
let create_church_booleans env =
  let true_fn = VFun (["x"; "y"], Var "x", env) in
  let false_fn = VFun (["x"; "y"], Var "y", env) in
  let if_fn = VFun (["c"; "t"; "e"], App (App (Var "c", Var "t"), Var "e"), env) in

  let env_with_primitives = env
    |> StringMap.add "true" true_fn
    |> StringMap.add "false" false_fn
    |> StringMap.add "if" if_fn
  in

  let not_fn = VFun (["b"], App (App (App (Var "if", Var "b"), Var "false"), Var "true"), env_with_primitives) in
  let and_fn = VFun (["p"; "q"], App (App (App (Var "if", Var "p"), Var "q"), Var "false"), env_with_primitives) in
  let or_fn = VFun (["p"; "q"], App (App (App (Var "if", Var "p"), Var "true"), Var "q"), env_with_primitives) in

  let final_env = env_with_primitives
    |> StringMap.add "not" not_fn
    |> StringMap.add "and" and_fn
    |> StringMap.add "or" or_fn
  in
  final_env

(* Define an eval_with_imports function to handle environment properly *)
let rec eval_with_imports env expr =
  match expr with
  | Import lib_name ->
      if StringMap.mem lib_name !imported_libraries then
        (env, VInt 0) (* Return unchanged env and dummy value *)
      else if lib_name = "operators" then (
        let final_env = create_church_booleans env in
        imported_libraries := StringMap.add lib_name true !imported_libraries;
        (final_env, VInt 0)
      )
      else (
        (* Load the library *)
        let lib_exprs = load_library lib_name in
        (* Mark as imported to prevent cycles *)
        imported_libraries := StringMap.add lib_name true !imported_libraries;

        (* Process all expressions in the library *)
        let new_env = List.fold_left
          (fun acc_env expr ->
            match expr with
            | FunDef (name, args, body) ->
                (* For Church-encoded functions, we need to create proper closures *)
                let func_val = VFun (args, body, acc_env) in
                StringMap.add name func_val acc_env
            | Let (name, _, value_expr) ->
                (* Evaluate the value and add to environment *)
                let (_, value) = eval_with_imports acc_env value_expr in
                StringMap.add name value acc_env
            | _ ->
                (* Evaluate other expressions but don't add to environment *)
                let (_, _) = eval_with_imports acc_env expr in
                acc_env
          ) env lib_exprs
        in

        (* Return the updated environment and a dummy value *)
        (new_env, VInt 0)
      )
  | Int n -> (env, VInt n)
  | Lng n -> (env, VLong n)
  | Float f -> (env, VFloat f)
  | Str s -> (env, VString s)
  | Add (a, b) ->
      let (env', va) = eval_with_imports env a in
      let (env'', vb) = eval_with_imports env' b in
      (env'',
       match va, vb with
       | VInt x, VInt y -> VInt (x + y)
       | VLong x, VLong y -> VLong (Int64.add x y)
       | VInt x, VLong y -> VLong (Int64.add (Int64.of_int x) y)
       | VLong x, VInt y -> VLong (Int64.add x (Int64.of_int y))
       | VFloat x, VFloat y -> VFloat (x +. y)
       | VInt x, VFloat y -> VFloat ((float_of_int x) +. y)
       | VFloat x, VInt y -> VFloat (x +. (float_of_int y))
       | _ -> raise (RuntimeError "Type error in add"))
  | Sub (a, b) ->
      let (env', va) = eval_with_imports env a in
      let (env'', vb) = eval_with_imports env' b in
      (env'',
       match va, vb with
       | VInt x, VInt y -> VInt (x - y)
       | VLong x, VLong y -> VLong (Int64.sub x y)
       | VInt x, VLong y -> VLong (Int64.sub (Int64.of_int x) y)
       | VLong x, VInt y -> VLong (Int64.sub x (Int64.of_int y))
       | VFloat x, VFloat y -> VFloat (x -. y)
       | VInt x, VFloat y -> VFloat ((float_of_int x) -. y)
       | VFloat x, VInt y -> VFloat (x -. (float_of_int y))
       | _ -> raise (RuntimeError "Type error in sub"))
  | Mul (a, b) ->
      let (env', va) = eval_with_imports env a in
      let (env'', vb) = eval_with_imports env' b in
      (env'',
       match va, vb with
       | VInt x, VInt y -> VInt (x * y)
       | VLong x, VLong y -> VLong (Int64.mul x y)
       | VInt x, VLong y -> VLong (Int64.mul (Int64.of_int x) y)
       | VLong x, VInt y -> VLong (Int64.mul x (Int64.of_int y))
       | VFloat x, VFloat y -> VFloat (x *. y)
       | VInt x, VFloat y -> VFloat ((float_of_int x) *. y)
       | VFloat x, VInt y -> VFloat (x *. (float_of_int y))
       | _ -> raise (RuntimeError "Type error in mul"))
  | Div (a, b) ->
      let (env', va) = eval_with_imports env a in
      let (env'', vb) = eval_with_imports env' b in
      (env'',
       match va, vb with
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
      let (env', va) = eval_with_imports env a in
      let (env'', vb) = eval_with_imports env' b in
      let result =
        match va, vb with
        | VInt x, VInt y -> x = y
        | VLong x, VLong y -> x = y
        | VString x, VString y -> x = y
        | _ -> false
      in
      (env'', VFun (["t"; "f"],
                     (if result then Var "t" else Var "f"),
                     env''))
  | Var x ->
      (try (env, StringMap.find x env)
       with Not_found -> raise (RuntimeError ("Unbound variable: " ^ x)))
  | Lam (x, body) -> (env, VFun ([x], body, env))
  | App (f, a) ->
      let (env', vf) = eval_with_imports env f in
      let (env'', va) = eval_with_imports env' a in
      (env'',
       match vf with
       | VFun ([], body, closure) ->
           (* Handle zero-parameter functions by evaluating the body directly *)
           let (_, result) = eval_with_imports closure body in
           result
       | VFun (x::xs, body, closure) ->
           let closure' = StringMap.add x va closure in
           if xs = [] then
             let (_, result) = eval_with_imports closure' body in
             result
           else VFun (xs, body, closure')
       | VPrim fn -> fn [va]
       | _ -> raise (RuntimeError "Attempt to call a non-function"))
  | Let (name, _, value) ->
      let (env', v) = eval_with_imports env value in
      (StringMap.add name v env', VFun ([], Var name, StringMap.add name v env'))
  | FunDef (name, args, body) ->
      let f = VFun (args, body, env) in
      (StringMap.add name f env, VFun ([], Var name, StringMap.add name f env))
  | Seq (a, b) ->
      let (env', _) = eval_with_imports env a in
      eval_with_imports env' b
  | Print e ->
      let (env', v) = eval_with_imports env e in
      let result =
        match v with
        | VFun ([], body, closure) ->
            (* Auto-invoke zero-parameter functions when printing *)
            let (_, v') = eval_with_imports closure body in
            v'
        | _ ->
            print_value v;
            v
      in
      (env', result)

(* Simple wrapper around eval_with_imports to keep original interface *)
let eval env expr =
  let (_, value) = eval_with_imports env expr in
  value

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
  | Import lib_name ->
      (* Process imports directly at toplevel to update the environment *)
      let (new_env, _) = eval_with_imports env (Import lib_name) in
      new_env
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
