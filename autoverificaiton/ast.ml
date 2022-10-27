open Sexplib.Std
module Ty = Normalty.Ast.T
open Normalty.Ast.Ntyped

type ty = Ty.t [@@deriving sexp]

type lit =
  | ACint of int
  | AVar of string typed
  | AOp2 of string * lit * lit
  | ACbool of bool
[@@deriving sexp]

type t =
  | Lit of lit
  | Implies of t * t
  | Ite of t * t * t
  | Not of t
  | And of t list
  | Or of t list
  | Iff of t * t
  | MethodPred of string * lit list
  | Forall of string typed * t
  | Exists of string typed * t
[@@deriving sexp]

let mk_true = Lit (ACbool true)
let mk_false = Lit (ACbool false)
let is_op = function "+" | "-" -> true | _ -> false

let is_bop = function
  | "==" | "!=" | "<" | ">" | "<=" | ">=" -> true
  | _ -> false

open Zzdatatype.Datatype

let count_qvs prop =
  let rec aux t =
    match t with
    | Lit _ -> 0
    | Implies (e1, e2) -> aux e1 + aux e2
    | Ite (e1, e2, e3) -> aux e1 + aux e2 + aux e3
    | Not e -> aux e
    | And es -> List.fold_left (fun sum e -> sum + aux e) 0 es
    | Or es -> List.fold_left (fun sum e -> sum + aux e) 0 es
    | Iff (e1, e2) -> aux e1 + aux e2
    | MethodPred (_, _) -> 0
    | Forall (_, e) -> 1 + aux e
    | Exists (_, e) -> 1 + aux e
  in
  aux prop

let rec lit_fv m t =
  match t with
  | ACint _ | ACbool _ -> m
  | AVar id -> StrMap.add id.x id.ty m
  | AOp2 (_, a, b) -> lit_fv (lit_fv m a) b

let _add_fv m prop =
  let rec aux m t =
    match t with
    | Lit lit -> lit_fv m lit
    | Implies (e1, e2) -> List.fold_left aux m [ e1; e2 ]
    | Ite (e1, e2, e3) -> List.fold_left aux m [ e1; e2; e3 ]
    | Not e -> aux m e
    | And es -> List.fold_left aux m es
    | Or es -> List.fold_left aux m es
    | Iff (e1, e2) -> List.fold_left aux m [ e1; e2 ]
    | MethodPred (_, args) -> List.fold_left lit_fv m args
    | Forall (u, e) -> StrMap.remove u.x @@ aux m e
    | Exists (u, e) -> StrMap.remove u.x @@ aux m e
  in
  aux m prop

let add_fv fv prop =
  let fv = StrMap.to_kv_list @@ _add_fv (StrMap.from_kv_list fv) prop in
  fv

let fv prop = fst @@ List.split @@ add_fv [] prop
let tvar_fv prop = List.map (fun (x, ty) -> { x; ty }) @@ add_fv [] prop

let negate prop =
  let rec aux t =
    match t with
    | Lit _ | MethodPred (_, _) -> Not t
    | Implies (e1, e2) -> And [ e1; aux e2 ]
    | Ite (e1, e2, e3) -> Ite (e1, aux e2, aux e3)
    | Not e -> e
    | And es -> Or (List.map aux es)
    | Or es -> And (List.map aux es)
    | Iff (e1, e2) -> Iff (e1, aux e2)
    | Forall (u, e) -> Exists (u, aux e)
    | Exists (u, e) -> Forall (u, aux e)
  in
  aux prop

let get_mps prop =
  let rec aux s t =
    match t with
    | Lit _ -> s
    | MethodPred (mp, AVar id :: _) -> { x = mp; ty = id.ty } :: s
    | MethodPred (_, _) -> s
    | Implies (e1, e2) -> aux (aux s e1) e2
    | Ite (e1, e2, e3) -> aux (aux (aux s e1) e2) e3
    | Not e -> aux s e
    | And es -> List.fold_left aux s es
    | Or es -> List.fold_left aux s es
    | Iff (e1, e2) -> aux (aux s e1) e2
    | Forall (_, e) -> aux s e
    | Exists (_, e) -> aux s e
  in
  let m = aux [] prop in
  List.slow_rm_dup typed_eq m

let count_mps prop = List.length @@ get_mps prop

let var_space_ prop =
  let rec aux_lit s = function
    | ACint _ | ACbool _ -> s
    | AVar x -> StrMap.add x.x x.ty s
    | AOp2 (_, arg1, arg2) -> aux_lit (aux_lit s arg1) arg2
  in
  let rec aux s t =
    match t with
    | Lit lit -> aux_lit s lit
    | MethodPred (_, args) -> List.fold_left aux_lit s args
    | Implies (e1, e2) -> aux (aux s e1) e2
    | Ite (e1, e2, e3) -> aux (aux (aux s e1) e2) e3
    | Not e -> aux s e
    | And es -> List.fold_left aux s es
    | Or es -> List.fold_left aux s es
    | Iff (e1, e2) -> aux (aux s e1) e2
    | Forall (x, e) -> StrMap.add x.x x.ty (aux s e)
    | Exists (x, e) -> StrMap.add x.x x.ty (aux s e)
  in
  let m = aux StrMap.empty prop in
  m

let var_space prop = StrMap.to_key_list (var_space_ prop)

let tvar_space prop =
  List.map (fun (x, ty) -> { x; ty }) @@ StrMap.to_kv_list (var_space_ prop)

let has_qv prop =
  let rec aux t =
    match t with
    | Lit _ | MethodPred (_, _) -> false
    | Implies (e1, e2) -> aux e1 || aux e2
    | Ite (e1, e2, e3) -> aux e1 || aux e2 || aux e3
    | Not e -> aux e
    | And es -> List.exists aux es
    | Or es -> List.exists aux es
    | Iff (e1, e2) -> aux e1 || aux e2
    | Forall (_, _) -> true
    | Exists (_, _) -> true
  in
  aux prop

let no_qv prop = not (has_qv prop)

let rec only_uq = function
  | Lit _ | MethodPred (_, _) -> true
  | Implies (e1, e2) -> no_qv e1 || only_uq e2
  | Ite (e1, e2, e3) -> no_qv e1 || only_uq e2 || only_uq e3
  | Not e -> no_qv e
  | And es -> List.exists only_uq es
  | Or es -> List.exists only_uq es
  | Iff (e1, e2) -> no_qv e1 || no_qv e2
  | Forall (_, e) -> only_uq e
  | Exists (_, _) -> false

let rec only_eq = function
  | Lit _ | MethodPred (_, _) -> true
  | Implies (e1, e2) -> no_qv e1 || only_eq e2
  | Ite (e1, e2, e3) -> no_qv e1 || only_eq e2 || only_eq e3
  | Not e -> no_qv e
  | And es -> List.exists only_eq es
  | Or es -> List.exists only_eq es
  | Iff (e1, e2) -> no_qv e1 || no_qv e2
  | Forall (_, _) -> false
  | Exists (_, e) -> only_eq e

(* TODO: type check *)
let lit_get_ty lit =
  let aux = function
    | ACint _ -> Ty.Ty_int
    | ACbool _ -> Ty.Ty_bool
    | AVar id -> id.ty
    | AOp2 (mp, _, _) -> (
        match mp with
        (* | "==" | "!=" | "<" | ">" | "<=" | ">=" -> Ty.Ty_bool *)
        | "+" | "-" -> Ty.Ty_int
        | _ -> failwith "lit_get_ty: unknown op")
  in
  aux lit

let typed_id_eq x y =
  if String.equal x.x y.x then
    let () = if eq x.ty y.ty then () else failwith "prop naming error" in
    true
  else false

let subst_lit_typed_id lit x y =
  let do_subst x y id = if typed_id_eq x id then y else id in
  let rec aux = function
    | ACint n -> ACint n
    | ACbool b -> ACbool b
    | AVar id -> AVar (do_subst x y id)
    | AOp2 (op, a, b) -> AOp2 (op, aux a, aux b)
  in
  aux lit

let subst_typed_id t x y =
  let rec aux t =
    match t with
    | Lit lit -> Lit (subst_lit_typed_id lit x y)
    | Implies (e1, e2) -> Implies (aux e1, aux e2)
    | Ite (e1, e2, e3) -> Ite (aux e1, aux e2, aux e3)
    | Not e -> Not (aux e)
    | And es -> And (List.map aux es)
    | Or es -> Or (List.map aux es)
    | Iff (e1, e2) -> Iff (aux e1, aux e2)
    | MethodPred (mp, args) ->
        MethodPred (mp, List.map (fun lit -> subst_lit_typed_id lit x y) args)
    | Forall (u, e) -> if typed_id_eq x u then t else Forall (u, aux e)
    | Exists (u, e) -> if typed_id_eq x u then t else Exists (u, aux e)
  in
  aux t

let subst_id_with_lit t x l =
  let rec aux_lit = function
    | ACint n -> ACint n
    | ACbool n -> ACbool n
    | AVar id when String.equal id.x x -> l
    | AVar id -> AVar id
    (* | AOp2 (op, AVar a, AVar b) -> *)
    (*     let () = *)
    (*       Printf.printf ">>>>>>>> %s %s %s -> %s %s %s\n" op a.x b.x op a.x *)
    (*         (match aux_lit (AVar b) with AVar b -> b.x | _ -> "unknown") *)
    (*     in *)
    (*     AOp2 (op, aux_lit (AVar a), aux_lit (AVar b)) *)
    | AOp2 (op, a, b) -> AOp2 (op, aux_lit a, aux_lit b)
  in
  let rec aux t =
    match t with
    | Lit lit -> Lit (aux_lit lit)
    | Implies (e1, e2) -> Implies (aux e1, aux e2)
    | Ite (e1, e2, e3) -> Ite (aux e1, aux e2, aux e3)
    | Not e -> Not (aux e)
    | And es -> And (List.map aux es)
    | Or es -> Or (List.map aux es)
    | Iff (e1, e2) -> Iff (aux e1, aux e2)
    | MethodPred (mp, args) -> MethodPred (mp, List.map aux_lit args)
    | Forall (u, e) -> if String.equal u.x x then t else Forall (u, aux e)
    | Exists (u, e) -> if String.equal u.x x then t else Exists (u, aux e)
  in
  aux t

let subst_lit_id lit x y =
  let do_subst x y id =
    if String.equal x id.x then { ty = id.ty; x = y } else id
  in
  let rec aux = function
    | ACint n -> ACint n
    | ACbool n -> ACbool n
    | AVar id -> AVar (do_subst x y id)
    | AOp2 (op, a, b) -> AOp2 (op, aux a, aux b)
  in
  aux lit

let subst_id t x y =
  let rec aux t =
    match t with
    | Lit lit -> Lit (subst_lit_id lit x y)
    | Implies (e1, e2) -> Implies (aux e1, aux e2)
    | Ite (e1, e2, e3) -> Ite (aux e1, aux e2, aux e3)
    | Not e -> Not (aux e)
    | And es -> And (List.map aux es)
    | Or es -> Or (List.map aux es)
    | Iff (e1, e2) -> Iff (aux e1, aux e2)
    | MethodPred (mp, args) ->
        MethodPred (mp, List.map (fun lit -> subst_lit_id lit x y) args)
    | Forall (u, e) -> if String.equal u.x x then t else Forall (u, aux e)
    | Exists (u, e) -> if String.equal u.x x then t else Exists (u, aux e)
  in
  aux t
