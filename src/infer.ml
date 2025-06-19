open Types
open Ast

exception TypeError of string

module StringMap = Map.Make(String)

type subst = (string * typ) list

let rec apply subst t =
  match t with
  | TVar v ->
      (try List.assoc v subst with Not_found -> t)
  | TFun (a, b) -> TFun (apply subst a, apply subst b)
  | _ -> t

let compose s1 s2 =
  List.map (fun (v, t) -> (v, apply s1 t)) s2 @ s1

let counter = ref 0
let fresh_var () =
  let v = Printf.sprintf "a%d" !counter in
  incr counter; TVar v

let rec occurs v = function
  | TVar v' -> v = v'
  | TFun (a, b) -> occurs v a || occurs v b
  | _ -> false

let rec unify t1 t2 =
  match t1, t2 with
  | TInt, TInt | TLong, TLong | TFloat, TFloat | TString, TString -> []
  | TVar v, t | t, TVar v ->
      if t = TVar v then [] else
      if occurs v t then raise (TypeError ("Occurs check: " ^ v))
      else [ (v, t) ]
  | TFun (a1, b1), TFun (a2, b2) ->
      let s1 = unify a1 a2 in
      let s2 = unify (apply s1 b1) (apply s1 b2) in
      compose s2 s1
  | _ -> raise (TypeError ("Cannot unify types: " ^ Ast.string_of_typ t1 ^ " and " ^ Ast.string_of_typ t2))

let generalize env t =
  let env_vars =
    StringMap.fold (fun _ t vs ->
      let rec vars acc = function
        | TVar v -> v :: acc
        | TFun (a, b) -> vars (vars acc a) b
        | _ -> acc
      in vars vs t
    ) env []
  in
  let rec vars acc = function
    | TVar v -> if List.mem v acc then acc else v :: acc
    | TFun (a, b) -> vars (vars acc a) b
    | _ -> acc
  in
  let tvars = vars [] t in
  List.filter (fun v -> not (List.mem v env_vars)) tvars

let add_operators_to_env env =
  let t_bool =
    let a = fresh_var () in
    TFun (a, TFun (a, a))
  in
  let env = StringMap.add "true" t_bool env in
  let env = StringMap.add "false" t_bool env in

  let t_if =
    let a = fresh_var () in
    TFun (t_bool, TFun (a, TFun (a, a)))
  in
  let env = StringMap.add "if" t_if env in

  let t_not = TFun (t_bool, t_bool) in
  let env = StringMap.add "not" t_not env in

  let t_and = TFun (t_bool, TFun (t_bool, t_bool)) in
  let env = StringMap.add "and" t_and env in

  let t_or = TFun (t_bool, TFun (t_bool, t_bool)) in
  let env = StringMap.add "or" t_or env in
  env

let instantiate t =
  let subst = ref [] in
  let rec aux = function
    | TVar v ->
        if List.mem_assoc v !subst then List.assoc v !subst
        else
          let v' = fresh_var () in
          subst := (v, v') :: !subst;
          v'
    | TFun (a, b) -> TFun (aux a, aux b)
    | t -> t
  in aux t

let rec infer env = function
  | Int _ -> ([], TInt)
  | Lng _ -> ([], TLong)
  | Float _ -> ([], TFloat)
  | Str _ -> ([], TString)
  | Add (a, b) | Sub (a, b) | Mul(a, b) ->
      let s1, t1 = infer env a in
      let s2, t2 = infer (StringMap.map (apply s1) env) b in
      let t1 = apply s2 t1 in
      let t2 = apply s2 t2 in
      let s3 = unify t1 t2 in
      (s3, apply s3 t1)
  | Div (a, b) ->
      let s1, t1 = infer env a in
      let s2, t2 = infer (StringMap.map (apply s1) env) b in
      let t1 = apply s2 t1 in
      let t2 = apply s2 t2 in
      let s3 = unify t1 t2 in
      if apply s3 t1 = TFloat || apply s3 t1 = TInt || apply s3 t1 = TLong then
        (s3, TFloat)
      else
        (s3, apply s3 t1)
  | Eq (a, b) ->
      let s1, t1 = infer env a in
      let s2, t2 = infer (StringMap.map (apply s1) env) b in
      let s3 = unify t1 t2 in
      let a = fresh_var () in
      let b = fresh_var () in
      (compose s3 (compose s2 s1), TFun (a, TFun (b, a)))
  | Var x ->
      (match StringMap.find_opt x env with
      | Some t -> ([], instantiate t)
      | None -> raise (TypeError ("Unbound variable: " ^ x)))
  | Lam (x, Lam (y, Var v)) when v = x ->
      let a = fresh_var () in
      let b = fresh_var () in
      ([], TFun (a, TFun (b, a)))
  | Lam (x, Lam (y, Var v)) when v = y ->
      let a = fresh_var () in
      let b = fresh_var () in
      ([], TFun (a, TFun (b, b)))
  | Lam (x, body) ->
      let tv = fresh_var () in
      let env' = StringMap.add x tv env in
      let s1, t1 = infer env' body in
      (s1, TFun (apply s1 tv, t1))
  | App (f, a) ->
      let s1, t1 = infer env f in
      let s2, t2 = infer (StringMap.map (apply s1) env) a in
      let tv = fresh_var () in
      let s3 = unify (apply s2 t1) (TFun (t2, tv)) in
      (compose s3 (compose s2 s1), apply s3 tv)
  | Let (name, typ, value) ->
      let s1, t1 = infer env value in
      (s1, t1)
  | FunDef (name, args, body) ->
      let arg_types = List.map (fun _ -> fresh_var ()) args in
      let env' = List.fold_left2 (fun e a t -> StringMap.add a t e) env args arg_types in
      let s1, t1 = infer env' body in
      let t = List.fold_right (fun a b -> TFun (a, b)) (List.map (apply s1) arg_types) t1 in
      (s1, t)
  | Seq (a, b) ->
      let s1, _ = infer env a in
      let s2, t2 = infer (StringMap.map (apply s1) env) b in
      (compose s2 s1, t2)
  | Print e ->
      let s1, t1 = infer env e in
      (s1, t1)
  | Import _ ->
      (* Import statements don't have a meaningful type themselves,
         they just make definitions available in the environment.
         Return unit type as a convention. *)
      ([], TInt)
