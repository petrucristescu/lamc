open Ast

module StringMap = Map.Make(String)

type value =
  | VInt of int
  | VLong of int64
  | VFloat of float
  | VString of string
  | VBool of bool
  | VFun of string list * expr * env
  | VRecFun of string * string list * expr * env  (* name * args * body * env — binds itself at call time *)
  | VPrim of (value list -> value)
  | VTailCall of env * expr  (* Trampoline marker for tail call optimization *)
and env = value StringMap.t

exception RuntimeError of string
exception AssertionFailure of string

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
        let lib_file = Filename.concat path (library_name ^ ".ch") in
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
        print_endline (string_of_int (int_of_float f))
      else
        print_endline (string_of_float f)
  | VString s -> print_endline s
  | VBool true -> print_endline "true"
  | VBool false -> print_endline "false"
  | VTailCall _ -> print_endline "<tailcall>"
  | VRecFun _ | _ -> print_endline "<fun>"

(* Boolean primitives using native VBool *)
let create_church_booleans env =
  let expect_bool name = function
    | VBool b -> b
    | _ -> raise (RuntimeError (name ^ ": expected boolean"))
  in
  let not_fn = VPrim (fun args -> VBool (not (expect_bool "not" (List.hd args)))) in
  let and_fn = VPrim (fun args ->
    let p = expect_bool "and" (List.hd args) in
    VPrim (fun args -> VBool (p && expect_bool "and" (List.hd args)))) in
  let or_fn = VPrim (fun args ->
    let p = expect_bool "or" (List.hd args) in
    VPrim (fun args -> VBool (p || expect_bool "or" (List.hd args)))) in
  let if_fn = VPrim (fun args ->
    let c = expect_bool "if" (List.hd args) in
    VPrim (fun args ->
      let t = List.hd args in
      VPrim (fun args ->
        let f = List.hd args in
        if c then t else f))) in
  env
  |> StringMap.add "true" (VBool true)
  |> StringMap.add "false" (VBool false)
  |> StringMap.add "not" not_fn
  |> StringMap.add "and" and_fn
  |> StringMap.add "or" or_fn
  |> StringMap.add "if" if_fn

(* Helper for binary numeric operations with type coercion *)
let eval_numeric_binop va vb int_op long_op float_op name =
  match va, vb with
  | VInt x, VInt y -> VInt (int_op x y)
  | VLong x, VLong y -> VLong (long_op x y)
  | VInt x, VLong y -> VLong (long_op (Int64.of_int x) y)
  | VLong x, VInt y -> VLong (long_op x (Int64.of_int y))
  | VFloat x, VFloat y -> VFloat (float_op x y)
  | VInt x, VFloat y -> VFloat (float_op (float_of_int x) y)
  | VFloat x, VInt y -> VFloat (float_op x (float_of_int y))
  | _ -> raise (RuntimeError ("Type error in " ^ name))

let eval_div_values va vb =
  let check_zero_int y = if y = 0 then raise (RuntimeError "Division by zero") in
  let check_zero_long y = if y = 0L then raise (RuntimeError "Division by zero") in
  let check_zero_float y = if y = 0.0 then raise (RuntimeError "Division by zero") in
  match va, vb with
  | VInt _, VInt 0 | VInt _, VLong 0L -> raise (RuntimeError "Division by zero")
  | VLong _, VInt 0 | VLong _, VLong 0L -> raise (RuntimeError "Division by zero")
  | VInt x, VInt y -> check_zero_int y; VFloat (float_of_int x /. float_of_int y)
  | VLong x, VLong y -> check_zero_long y; VFloat (Int64.to_float x /. Int64.to_float y)
  | VInt x, VLong y -> check_zero_long y; VFloat (float_of_int x /. Int64.to_float y)
  | VLong x, VInt y -> check_zero_int y; VFloat (Int64.to_float x /. float_of_int y)
  | VFloat x, VFloat y -> check_zero_float y; VFloat (x /. y)
  | VInt x, VFloat y -> check_zero_float y; VFloat (float_of_int x /. y)
  | VFloat x, VInt y -> check_zero_int y; VFloat (x /. float_of_int y)
  | _ -> raise (RuntimeError "Type error in div")

(* Define an eval_with_imports function to handle environment properly *)
(* eval_with_imports and force are mutually recursive for tail call optimization *)
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
                let func_val = VRecFun (name, args, body, acc_env) in
                StringMap.add name func_val acc_env
            | Let (name, _, value_expr) ->
                (* Evaluate the value and add to environment *)
                let (_, value) = eval_with_imports acc_env value_expr in
                StringMap.add name (force value) acc_env
            | _ ->
                (* Evaluate other expressions but don't add to environment *)
                let (_, v) = eval_with_imports acc_env expr in
                ignore (force v);
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
  | Bool b -> (env, VBool b)
  | Add (a, b) -> eval_binop env a b ( + ) Int64.add ( +. ) "add"
  | Sub (a, b) -> eval_binop env a b ( - ) Int64.sub ( -. ) "sub"
  | Mul (a, b) -> eval_binop env a b ( * ) Int64.mul ( *. ) "mul"
  | Div (a, b) -> eval_div env a b
  | Eq (a, b) ->
      let (env', va) = eval_with_imports env a in
      let va = force va in
      let (env'', vb) = eval_with_imports env' b in
      let vb = force vb in
      let result =
        match va, vb with
        | VInt x, VInt y -> x = y
        | VLong x, VLong y -> x = y
        | VInt x, VLong y -> Int64.of_int x = y
        | VLong x, VInt y -> x = Int64.of_int y
        | VString x, VString y -> x = y
        | VBool x, VBool y -> x = y
        | _ -> false
      in
      (env'', VBool result)
  | Var x ->
      (try (env, StringMap.find x env)
       with Not_found -> raise (RuntimeError ("Unbound variable: " ^ x)))
  | Lam (x, body) -> (env, VFun ([x], body, env))
  | App (f, a) ->
      let (env', vf) = eval_with_imports env f in
      let vf = force vf in
      let (env'', va) = eval_with_imports env' a in
      let va = force va in
      (env'',
       match vf with
       | VFun ([], body, closure) ->
           (* Tail call: defer body evaluation *)
           VTailCall (closure, body)
       | VFun (x::xs, body, closure) ->
           let closure' = StringMap.add x va closure in
           if xs = [] then
             (* Tail call: defer body evaluation *)
             VTailCall (closure', body)
           else VFun (xs, body, closure')
       | VRecFun (name, x::xs, body, closure) ->
           (* Bind the function's own name in its closure for recursion *)
           let self = VRecFun (name, x::xs, body, closure) in
           let closure' = StringMap.add name self (StringMap.add x va closure) in
           if xs = [] then
             (* Tail call: defer body evaluation *)
             VTailCall (closure', body)
           else VFun (xs, body, closure')
       | VBool b ->
           (* Booleans are applicable: true t f = t, false t f = f *)
           let first_arg = va in
           VPrim (fun args -> if b then first_arg else List.hd args)
       | VPrim fn -> fn [va]
       | _ -> raise (RuntimeError "Attempt to call a non-function"))
  | Let (name, _, value) ->
      let (env', v) = eval_with_imports env value in
      let v = force v in
      (StringMap.add name v env', v)
  | FunDef (name, args, body) ->
      let f = if args = [] then VFun (args, body, env)
              else VRecFun (name, args, body, env) in
      let env' = StringMap.add name f env in
      (env', f)
  | Seq (a, b) ->
      let (env', va) = eval_with_imports env a in
      ignore (force va); (* Force first expression for side effects *)
      eval_with_imports env' b
  | Print e ->
      let (env', v) = eval_with_imports env e in
      let v = force v in
      let result =
        match v with
        | VFun ([], body, closure) ->
            (* Auto-invoke zero-parameter functions when printing *)
            let (_, v') = eval_with_imports closure body in
            force v'
        | _ ->
            print_value v;
            v
      in
      (env', result)
  | Assert e ->
      let (env', v) = eval_with_imports env e in
      let v = force v in
      (match v with
       | VBool true -> (env', VBool true)
       | VBool false ->
           raise (AssertionFailure ("Assertion failed: " ^ Ast.string_of_expr e))
       | _ ->
           raise (AssertionFailure ("Assert requires a boolean, got: " ^ Ast.string_of_expr e)))

(* Trampoline: resolve VTailCall chains without growing the stack.
   force is tail-recursive, so OCaml optimizes it into a loop. *)
and eval_binop env a b int_op long_op float_op name =
  let (env', va) = eval_with_imports env a in
  let va = force va in
  let (env'', vb) = eval_with_imports env' b in
  let vb = force vb in
  (env'', eval_numeric_binop va vb int_op long_op float_op name)

and eval_div env a b =
  let (env', va) = eval_with_imports env a in
  let va = force va in
  let (env'', vb) = eval_with_imports env' b in
  let vb = force vb in
  (env'', eval_div_values va vb)

and force = function
  | VTailCall (tc_env, tc_expr) ->
      force (snd (eval_with_imports tc_env tc_expr))
  | v -> v

(* Simple wrapper around eval_with_imports to keep original interface *)
let eval env expr =
  let (_, value) = eval_with_imports env expr in
  force value

let rec eval_toplevel (env : env) (expr : expr) : env =
  match expr with
  | Seq (a, b) ->
      let env' = eval_toplevel env a in
      eval_toplevel env' b
  | Let (name, _, value) ->
      let v = eval env value in
      StringMap.add name v env
  | FunDef (name, args, body) ->
      let f = if args = [] then VFun (args, body, env)
              else VRecFun (name, args, body, env) in
      StringMap.add name f env
  | Import lib_name ->
      (* Process imports directly at toplevel to update the environment *)
      let (new_env, _) = eval_with_imports env (Import lib_name) in
      new_env
  | _ ->
      ignore (eval env expr);
      env

let eval_program exprs =
  imported_libraries := StringMap.empty;
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
