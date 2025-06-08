type typ =
  | TInt
  | TLong
  | TString
  | TVar of string
  | TFun of typ * typ
  | TUnknown

type scheme = Forall of string list * typ

module StringMap = Map.Make(String)
type type_env = scheme StringMap.t

let free_type_vars_typ t =
  let rec aux acc = function
    | TVar v -> if List.mem v acc then acc else v :: acc
    | TFun (a, b) -> aux (aux acc a) b
    | _ -> acc
  in aux [] t

let free_type_vars_scheme (Forall (vars, t)) =
  List.filter (fun v -> not (List.mem v vars)) (free_type_vars_typ t)

let free_type_vars_env env =
  StringMap.fold (fun _ scheme acc ->
    List.fold_left (fun acc v -> if List.mem v acc then acc else v :: acc)
      acc (free_type_vars_scheme scheme)
  ) env []

let generalize env t =
  let env_vars = free_type_vars_env env in
  let t_vars = free_type_vars_typ t in
  let vars = List.filter (fun v -> not (List.mem v env_vars)) t_vars in
  Forall (vars, t)

let instantiate (Forall (vars, t)) =
  let subst = List.fold_left (fun acc v -> (v, TVar (v ^ "'")) :: acc) [] vars in
  let rec subst_typ = function
    | TVar v as tv -> (try List.assoc v subst with Not_found -> tv)
    | TFun (a, b) -> TFun (subst_typ a, subst_typ b)
    | t -> t
  in subst_typ t