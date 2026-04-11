open Ast

module StringMap = Map.Make(String)

type value =
  | VInt of int
  | VLong of int64
  | VFloat of float
  | VString of string
  | VBool of bool
  | VList of value list
  | VCons of value * value
  | VNil
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

(* String representation of a value (without newline) *)
let rec string_of_value = function
  | VInt n -> string_of_int n
  | VLong n -> Int64.to_string n
  | VFloat f ->
      if Float.equal (Float.round f) f then string_of_int (int_of_float f)
      else string_of_float f
  | VString s -> s
  | VBool true -> "true"
  | VBool false -> "false"
  | VList vs -> "[" ^ String.concat ", " (List.map string_of_value vs) ^ "]"
  | VCons _ as c -> string_of_cons c
  | VNil -> "nil"
  | VTailCall _ -> "<tailcall>"
  | VRecFun _ | _ -> "<fun>"

and string_of_cons v =
  let rec collect acc = function
    | VCons (h, t) -> collect (h :: acc) t
    | VNil -> "[" ^ String.concat ", " (List.rev_map string_of_value acc) ^ "]"
    | tail -> "(" ^ String.concat ", " (List.rev_map string_of_value acc) ^ " . " ^ string_of_value tail ^ ")"
  in
  collect [] v

(* Print function for values *)
let print_value v = print_endline (string_of_value v)

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

(* Math primitives *)
let create_math_functions env =
  let expect_numeric name = function
    | VInt n -> float_of_int n
    | VLong n -> Int64.to_float n
    | VFloat f -> f
    | _ -> raise (RuntimeError (name ^ ": expected numeric"))
  in
  let math1 name f = VPrim (fun args -> VFloat (f (expect_numeric name (List.hd args)))) in
  let math2 name f = VPrim (fun args ->
    let a = expect_numeric name (List.hd args) in
    VPrim (fun args -> VFloat (f a (expect_numeric name (List.hd args))))) in
  env
  |> StringMap.add "sqrt" (math1 "sqrt" Float.sqrt)
  |> StringMap.add "sin" (math1 "sin" Float.sin)
  |> StringMap.add "cos" (math1 "cos" Float.cos)
  |> StringMap.add "tan" (math1 "tan" Float.tan)
  |> StringMap.add "asin" (math1 "asin" Float.asin)
  |> StringMap.add "acos" (math1 "acos" Float.acos)
  |> StringMap.add "atan" (math1 "atan" Float.atan)
  |> StringMap.add "floor" (VPrim (fun args ->
      VInt (int_of_float (Float.floor (expect_numeric "floor" (List.hd args))))))
  |> StringMap.add "ceil" (VPrim (fun args ->
      VInt (int_of_float (Float.ceil (expect_numeric "ceil" (List.hd args))))))
  |> StringMap.add "round" (VPrim (fun args ->
      VInt (int_of_float (Float.round (expect_numeric "round" (List.hd args))))))
  |> StringMap.add "abs" (VPrim (fun args ->
      match List.hd args with
      | VInt n -> VInt (abs n)
      | VLong n -> VLong (Int64.abs n)
      | VFloat f -> VFloat (Float.abs f)
      | _ -> raise (RuntimeError "abs: expected numeric")))
  |> StringMap.add "pow" (math2 "pow" Float.pow)
  |> StringMap.add "min" (VPrim (fun args ->
      let a = expect_numeric "min" (List.hd args) in
      VPrim (fun args -> VFloat (Float.min a (expect_numeric "min" (List.hd args))))))
  |> StringMap.add "max" (VPrim (fun args ->
      let a = expect_numeric "max" (List.hd args) in
      VPrim (fun args -> VFloat (Float.max a (expect_numeric "max" (List.hd args))))))

(* String primitives *)
let create_string_functions env =
  let expect_string name = function
    | VString s -> s
    | _ -> raise (RuntimeError (name ^ ": expected string"))
  in
  let expect_int name = function
    | VInt n -> n
    | _ -> raise (RuntimeError (name ^ ": expected int"))
  in
  env
  |> StringMap.add "length" (VPrim (fun args ->
      VInt (String.length (expect_string "length" (List.hd args)))))
  |> StringMap.add "concat" (VPrim (fun args ->
      let s1 = expect_string "concat" (List.hd args) in
      VPrim (fun args -> VString (s1 ^ expect_string "concat" (List.hd args)))))
  |> StringMap.add "substring" (VPrim (fun args ->
      let s = expect_string "substring" (List.hd args) in
      VPrim (fun args ->
        let start = expect_int "substring" (List.hd args) in
        VPrim (fun args ->
          let len = expect_int "substring" (List.hd args) in
          if start < 0 || start + len > String.length s then
            raise (RuntimeError "substring: index out of bounds")
          else VString (String.sub s start len)))))
  |> StringMap.add "uppercase" (VPrim (fun args ->
      VString (String.uppercase_ascii (expect_string "uppercase" (List.hd args)))))
  |> StringMap.add "lowercase" (VPrim (fun args ->
      VString (String.lowercase_ascii (expect_string "lowercase" (List.hd args)))))
  |> StringMap.add "trim" (VPrim (fun args ->
      VString (String.trim (expect_string "trim" (List.hd args)))))
  |> StringMap.add "charAt" (VPrim (fun args ->
      let s = expect_string "charAt" (List.hd args) in
      VPrim (fun args ->
        let i = expect_int "charAt" (List.hd args) in
        if i < 0 || i >= String.length s then
          raise (RuntimeError "charAt: index out of bounds")
        else VString (String.make 1 s.[i]))))
  |> StringMap.add "indexOf" (VPrim (fun args ->
      let haystack = expect_string "indexOf" (List.hd args) in
      VPrim (fun args ->
        let needle = expect_string "indexOf" (List.hd args) in
        let len_h = String.length haystack in
        let len_n = String.length needle in
        let rec search i =
          if i > len_h - len_n then VInt (-1)
          else if String.sub haystack i len_n = needle then VInt i
          else search (i + 1)
        in
        if len_n = 0 then VInt 0
        else if len_n > len_h then VInt (-1)
        else search 0)))
  |> StringMap.add "startsWith" (VPrim (fun args ->
      let s = expect_string "startsWith" (List.hd args) in
      VPrim (fun args ->
        let prefix = expect_string "startsWith" (List.hd args) in
        let len = String.length prefix in
        VBool (len <= String.length s && String.sub s 0 len = prefix))))
  |> StringMap.add "endsWith" (VPrim (fun args ->
      let s = expect_string "endsWith" (List.hd args) in
      VPrim (fun args ->
        let suffix = expect_string "endsWith" (List.hd args) in
        let slen = String.length s in
        let suflen = String.length suffix in
        VBool (suflen <= slen && String.sub s (slen - suflen) suflen = suffix))))
  |> StringMap.add "replace" (VPrim (fun args ->
      let s = expect_string "replace" (List.hd args) in
      VPrim (fun args ->
        let old_s = expect_string "replace" (List.hd args) in
        VPrim (fun args ->
          let new_s = expect_string "replace" (List.hd args) in
          let old_len = String.length old_s in
          if old_len = 0 then VString s
          else
            let buf = Buffer.create (String.length s) in
            let rec aux i =
              if i >= String.length s then ()
              else if i + old_len <= String.length s && String.sub s i old_len = old_s then
                (Buffer.add_string buf new_s; aux (i + old_len))
              else (Buffer.add_char buf s.[i]; aux (i + 1))
            in
            aux 0; VString (Buffer.contents buf)))))
  |> StringMap.add "toString" (VPrim (fun args ->
      match List.hd args with
      | VString s -> VString s
      | VInt n -> VString (string_of_int n)
      | VLong n -> VString (Int64.to_string n)
      | VFloat f ->
          if Float.equal (Float.round f) f then VString (string_of_int (int_of_float f))
          else VString (string_of_float f)
      | VBool true -> VString "true"
      | VBool false -> VString "false"
      | v -> VString (string_of_value v)))

(* Date/Time primitives *)
let create_time_functions env =
  let expect_int name = function
    | VInt n -> n
    | _ -> raise (RuntimeError (name ^ ": expected int"))
  in
  let tm_of_ts ts = Unix.localtime (float_of_int ts) in
  env
  |> StringMap.add "now" (VPrim (fun _ -> VInt (int_of_float (Unix.time ()))))
  |> StringMap.add "timeMs" (VPrim (fun _ -> VInt (int_of_float (Unix.gettimeofday () *. 1000.0))))
  |> StringMap.add "year" (VPrim (fun args ->
      let tm = tm_of_ts (expect_int "year" (List.hd args)) in
      VInt (tm.Unix.tm_year + 1900)))
  |> StringMap.add "month" (VPrim (fun args ->
      let tm = tm_of_ts (expect_int "month" (List.hd args)) in
      VInt (tm.Unix.tm_mon + 1)))
  |> StringMap.add "day" (VPrim (fun args ->
      let tm = tm_of_ts (expect_int "day" (List.hd args)) in
      VInt tm.Unix.tm_mday))
  |> StringMap.add "hour" (VPrim (fun args ->
      let tm = tm_of_ts (expect_int "hour" (List.hd args)) in
      VInt tm.Unix.tm_hour))
  |> StringMap.add "minute" (VPrim (fun args ->
      let tm = tm_of_ts (expect_int "minute" (List.hd args)) in
      VInt tm.Unix.tm_min))
  |> StringMap.add "second" (VPrim (fun args ->
      let tm = tm_of_ts (expect_int "second" (List.hd args)) in
      VInt tm.Unix.tm_sec))
  |> StringMap.add "diffTime" (VPrim (fun args ->
      let a = expect_int "diffTime" (List.hd args) in
      VPrim (fun args ->
        let b = expect_int "diffTime" (List.hd args) in
        VInt (a - b))))
  |> StringMap.add "dayOfWeek" (VPrim (fun args ->
      let tm = tm_of_ts (expect_int "dayOfWeek" (List.hd args)) in
      VInt tm.Unix.tm_wday))

(* List primitives *)
(* Forward reference for eval — set by eval_with_imports at startup *)
let eval_ref : (env -> expr -> env * value) ref = ref (fun _ _ -> failwith "eval not initialized")
let force_ref : (value -> value) ref = ref (fun v -> v)

let apply_fn f arg =
  match f with
  | VPrim fn -> fn [arg]
  | VFun (x::xs, body, closure) ->
      let closure' = StringMap.add x arg closure in
      if xs = [] then !force_ref (snd (!eval_ref closure' body))
      else VFun (xs, body, closure')
  | VRecFun (name, x::xs, body, closure) ->
      let self = VRecFun (name, x::xs, body, closure) in
      let closure' = StringMap.add name self (StringMap.add x arg closure) in
      if xs = [] then !force_ref (snd (!eval_ref closure' body))
      else VFun (xs, body, closure')
  | _ -> raise (RuntimeError "apply: expected function")

let create_list_functions env =
  let expect_list name = function
    | VList vs -> vs
    | VNil -> []
    | _ -> raise (RuntimeError (name ^ ": expected list"))
  in
  let expect_int name = function
    | VInt n -> n
    | _ -> raise (RuntimeError (name ^ ": expected int"))
  in
  env
  |> StringMap.add "nil" VNil
  |> StringMap.add "cons" (VPrim (fun args ->
      let h = List.hd args in
      VPrim (fun args ->
        match List.hd args with
        | VList vs -> VList (h :: vs)
        | VNil -> VList [h]
        | t -> VCons (h, t))))
  |> StringMap.add "head" (VPrim (fun args ->
      match expect_list "head" (List.hd args) with
      | x :: _ -> x
      | [] -> raise (RuntimeError "head: empty list")))
  |> StringMap.add "tail" (VPrim (fun args ->
      match expect_list "tail" (List.hd args) with
      | _ :: xs -> VList xs
      | [] -> raise (RuntimeError "tail: empty list")))
  |> StringMap.add "empty" (VPrim (fun args ->
      VBool (expect_list "empty" (List.hd args) = [])))
  |> StringMap.add "len" (VPrim (fun args ->
      VInt (List.length (expect_list "len" (List.hd args)))))
  |> StringMap.add "nth" (VPrim (fun args ->
      let l = expect_list "nth" (List.hd args) in
      VPrim (fun args ->
        let n = expect_int "nth" (List.hd args) in
        if n < 0 || n >= List.length l then raise (RuntimeError "nth: index out of bounds")
        else List.nth l n)))
  |> StringMap.add "reverse" (VPrim (fun args ->
      VList (List.rev (expect_list "reverse" (List.hd args)))))
  |> StringMap.add "range" (VPrim (fun args ->
      let lo = expect_int "range" (List.hd args) in
      VPrim (fun args ->
        let hi = expect_int "range" (List.hd args) in
        let rec build i acc = if i < lo then acc else build (i-1) (VInt i :: acc) in
        VList (build (hi - 1) []))))
  |> StringMap.add "map" (VPrim (fun args ->
      let f = List.hd args in
      VPrim (fun args ->
        let l = expect_list "map" (List.hd args) in
        VList (List.map (apply_fn f) l))))
  |> StringMap.add "filter" (VPrim (fun args ->
      let f = List.hd args in
      VPrim (fun args ->
        let l = expect_list "filter" (List.hd args) in
        VList (List.filter (fun v ->
          match apply_fn f v with
          | VBool b -> b
          | _ -> raise (RuntimeError "filter: predicate must return bool")
        ) l))))
  |> StringMap.add "foldl" (VPrim (fun args ->
      let f = List.hd args in
      VPrim (fun args ->
        let init = List.hd args in
        VPrim (fun args ->
          let l = expect_list "foldl" (List.hd args) in
          List.fold_left (fun acc v -> apply_fn (apply_fn f acc) v) init l))))
  |> StringMap.add "foldr" (VPrim (fun args ->
      let f = List.hd args in
      VPrim (fun args ->
        let init = List.hd args in
        VPrim (fun args ->
          let l = expect_list "foldr" (List.hd args) in
          List.fold_right (fun v acc -> apply_fn (apply_fn f v) acc) l init))))
  (* Church-style eliminators: matchList value on_empty on_cons *)
  |> StringMap.add "matchList" (VPrim (fun args ->
      let l = List.hd args in
      VPrim (fun args ->
        let on_empty = List.hd args in
        VPrim (fun args ->
          let on_cons = List.hd args in
          match l with
          | VList [] | VNil -> apply_fn on_empty VNil
          | VList (h :: t) -> apply_fn (apply_fn on_cons h) (VList t)
          | _ -> raise (RuntimeError "matchList: expected list")))))
  (* matchBool: matchBool cond on_true on_false — Scott-encoded boolean eliminator *)
  |> StringMap.add "matchBool" (VPrim (fun args ->
      match List.hd args with
      | VBool true -> VPrim (fun args -> let t = List.hd args in VPrim (fun _ -> t))
      | VBool false -> VPrim (fun _ -> VPrim (fun args -> List.hd args))
      | _ -> raise (RuntimeError "matchBool: expected bool")))

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

(* Pattern matching — returns Some bindings or None *)
let rec match_pattern (pat : Ast.pattern) (v : value) : (string * value) list option =
  match pat, v with
  | Ast.PWild, _ -> Some []
  | Ast.PVar x, _ -> Some [(x, v)]
  | Ast.PInt n, VInt m when n = m -> Some []
  | Ast.PStr s, VString t when s = t -> Some []
  | Ast.PBool b, VBool c when b = c -> Some []
  | Ast.PList pats, VList vs ->
      if List.length pats <> List.length vs then None
      else
        let rec zip ps vs acc =
          match ps, vs with
          | [], [] -> Some acc
          | p :: ps', v :: vs' ->
              (match match_pattern p v with
               | Some binds -> zip ps' vs' (acc @ binds)
               | None -> None)
          | _ -> None
        in
        zip pats vs []
  | Ast.PList [], VNil -> Some []
  | Ast.PCons (hp, tp), VList (h :: t) ->
      (match match_pattern hp h with
       | Some hb ->
           (match match_pattern tp (VList t) with
            | Some tb -> Some (hb @ tb)
            | None -> None)
       | None -> None)
  | Ast.PCons (hp, tp), VCons (h, t) ->
      (match match_pattern hp h with
       | Some hb ->
           (match match_pattern tp t with
            | Some tb -> Some (hb @ tb)
            | None -> None)
       | None -> None)
  | _ -> None

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
      else if lib_name = "math" then (
        let final_env = create_math_functions env in
        imported_libraries := StringMap.add lib_name true !imported_libraries;
        (final_env, VInt 0)
      )
      else if lib_name = "string" then (
        let final_env = create_string_functions env in
        imported_libraries := StringMap.add lib_name true !imported_libraries;
        (final_env, VInt 0)
      )
      else if lib_name = "list" then (
        let final_env = create_list_functions env in
        imported_libraries := StringMap.add lib_name true !imported_libraries;
        (final_env, VInt 0)
      )
      else if lib_name = "time" then (
        let final_env = create_time_functions env in
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
            | Let (name, value_expr) ->
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
  | List items ->
      let env', vs = List.fold_left (fun (e, acc) item ->
        let e', v = eval_with_imports e item in
        (e', acc @ [force v])
      ) (env, []) items in
      (env', VList vs)
  | Add (a, b) -> eval_binop env a b ( + ) Int64.add ( +. ) "add"
  | Sub (a, b) -> eval_binop env a b ( - ) Int64.sub ( -. ) "sub"
  | Mul (a, b) -> eval_binop env a b ( * ) Int64.mul ( *. ) "mul"
  | Div (a, b) -> eval_div env a b
  | Eq (a, b) ->
      let (env', va) = eval_with_imports env a in
      let va = force va in
      let (env'', vb) = eval_with_imports env' b in
      let vb = force vb in
      let rec values_equal a b =
        match a, b with
        | VInt x, VInt y -> x = y
        | VLong x, VLong y -> x = y
        | VInt x, VLong y -> Int64.of_int x = y
        | VLong x, VInt y -> x = Int64.of_int y
        | VString x, VString y -> x = y
        | VBool x, VBool y -> x = y
        | VList xs, VList ys ->
            List.length xs = List.length ys && List.for_all2 values_equal xs ys
        | VCons (h1, t1), VCons (h2, t2) ->
            values_equal h1 h2 && values_equal t1 t2
        | VNil, VNil -> true
        | _ -> false
      in
      let result = values_equal va vb in
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
  | Let (name, value) ->
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
  | Match (scrutinee, arms) ->
      let (env', sv) = eval_with_imports env scrutinee in
      let sv = force sv in
      let rec try_arms = function
        | [] -> raise (RuntimeError ("No matching pattern for: " ^ string_of_value sv))
        | (pat, body) :: rest ->
            (match match_pattern pat sv with
             | Some bindings ->
                 let env'' = List.fold_left (fun e (k, v) -> StringMap.add k v e) env' bindings in
                 eval_with_imports env'' body
             | None -> try_arms rest)
      in
      try_arms arms
  | Try (expr, handler) ->
      (try
        let (env', v) = eval_with_imports env expr in
        (env', force v)
      with
      | RuntimeError msg ->
          let (_, hv) = eval_with_imports env handler in
          let hv = force hv in
          (env, apply_fn hv (VString msg))
      | AssertionFailure msg ->
          let (_, hv) = eval_with_imports env handler in
          let hv = force hv in
          (env, apply_fn hv (VString msg))
      | Division_by_zero ->
          let (_, hv) = eval_with_imports env handler in
          let hv = force hv in
          (env, apply_fn hv (VString "Division by zero")))

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

(* Initialize forward references for list operations *)
let () = eval_ref := eval_with_imports
let () = force_ref := force

(* Simple wrapper around eval_with_imports to keep original interface *)
let eval env expr =
  let (_, value) = eval_with_imports env expr in
  force value

let rec eval_toplevel (env : env) (expr : expr) : env =
  match expr with
  | Seq (a, b) ->
      let env' = eval_toplevel env a in
      eval_toplevel env' b
  | Let (name, value) ->
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
      print_endline ("\nChuring Error: " ^ msg);
      (* Don't re-raise the exception, just gracefully terminate *)
      ()
  | Division_by_zero ->
      print_endline "\nChuring Error: Division by zero"
      (* Don't re-raise the exception, just gracefully terminate *)
