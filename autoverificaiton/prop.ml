include Ast
open Normalty.Ast.Ntyped

let bvar_to_prop x = Lit (AVar x)

(*FV*)
open Zzdatatype.Datatype

let rec lit_fv m t =
  match t with
  | ACint _ | ACbool _ -> m
  | AVar id -> StrMap.add id.x () m
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
  let fv =
    StrMap.to_key_list
    @@ _add_fv
         (StrMap.from_kv_list @@ List.map (fun name -> (name, ())) fv)
         prop
  in
  fv

let fv prop = add_fv [] prop

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
  let open Zzdatatype.Datatype in
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

let var_space prop =
  let open Zzdatatype.Datatype in
  let rec aux_lit s = function
    | ACint _ | ACbool _ -> s
    | AVar x -> StrMap.add x.x () s
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
    | Forall (x, e) -> StrMap.add x.x () (aux s e)
    | Exists (x, e) -> StrMap.add x.x () (aux s e)
  in
  let m = aux StrMap.empty prop in
  StrMap.to_key_list m

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

let mk_mp_vars mp args = MethodPred (mp, List.map (fun x -> AVar x) args)

let mk_forall (ty, x) prop =
  let u = { ty; x } in
  Forall (u, prop u)

let mk_exists (ty, x) prop =
  let u = { ty; x } in
  Exists (u, prop u)

let mk_lit_eq_lit lit1 lit2 = MethodPred ("==", [ lit1; lit2 ])

let lit_strict_eq l1 l2 =
  let rec aux (l1, l2) =
    match (l1, l2) with
    | ACint n, ACint n' -> n == n'
    | ACbool n, ACbool n' -> n == n'
    | AVar id, AVar id' -> typed_id_eq id id'
    | AOp2 (mp, a, b), AOp2 (mp', a', b') ->
        String.equal mp mp' && aux (a, a') && aux (b, b')
    | _, _ -> false
  in
  aux (l1, l2)

let strict_eq t1 t2 =
  let rec aux (t1, t2) =
    match (t1, t2) with
    | Lit lit, Lit lit' -> lit_strict_eq lit lit'
    | Implies (e1, e2), Implies (e1', e2') -> aux (e1, e1') && aux (e2, e2')
    | Ite (e1, e2, e3), Ite (e1', e2', e3') ->
        aux (e1, e1') && aux (e2, e2') && aux (e3, e3')
    | Not e, Not e' -> aux (e, e')
    | And es, And es' when List.length es = List.length es' ->
        List.for_all aux @@ List.combine es es'
    | Or es, Or es' when List.length es = List.length es' ->
        List.for_all aux @@ List.combine es es'
    | Iff (e1, e2), Iff (e1', e2') -> aux (e1, e1') && aux (e2, e2')
    | MethodPred (mp, args), MethodPred (mp', args')
      when List.length args = List.length args' ->
        String.equal mp mp'
        && List.for_all (fun (l, l') -> lit_strict_eq l l')
           @@ List.combine args args'
    | Forall (u, e), Forall (u', e') -> typed_id_eq u u' && aux (e, e')
    | Exists (u, e), Exists (u', e') -> typed_id_eq u u' && aux (e, e')
    | _, _ -> false
  in
  aux (t1, t2)

let eq = strict_eq

let instantiate_vars (y, lit) t =
  let rec aux_lit t =
    match t with
    | ACint _ -> t
    | ACbool _ -> t
    | AVar id -> if String.equal id.x y then lit else t
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
    | MethodPred (mp, args) ->
        MethodPred (mp, List.map (fun lit -> aux_lit lit) args)
    | Forall (u, e) -> if String.equal u.x y then t else Forall (u, aux e)
    | Exists (u, e) -> if String.equal u.x y then t else Exists (u, aux e)
  in
  aux t

let simp_conj_disj t =
  let is_bool b' = function Lit (ACbool b) -> b == b' | _ -> false in
  let rec aux t =
    match t with
    | Lit _ | MethodPred (_, _) -> t
    | Implies (e1, e2) -> Implies (aux e1, aux e2)
    | Ite (e1, e2, e3) -> Ite (aux e1, aux e2, aux e3)
    | Not (Lit (ACbool b)) -> Lit (ACbool (not b))
    | Not e -> Not (aux e)
    | And es -> (
        let es = List.map aux es in
        if List.exists (is_bool false) es then Lit (ACbool false)
        else
          let es = List.filter (fun x -> not (is_bool true x)) es in
          match es with [] -> Lit (ACbool true) | _ -> And es)
    | Or es -> (
        let es = List.map aux es in
        if List.exists (is_bool true) es then Lit (ACbool true)
        else
          let es = List.filter (fun x -> not (is_bool false x)) es in
          match es with [] -> Lit (ACbool false) | _ -> Or es)
    | Iff (e1, e2) -> Iff (aux e1, aux e2)
    | Forall (u, e) -> Forall (u, aux e)
    | Exists (u, e) -> Exists (u, aux e)
  in
  aux t

let add_with_simp_eq_prop id xprop prop =
  let is_eq = function
    | MethodPred ("==", [ AVar x; lit ]) when String.equal x.x id.x -> Some lit
    | MethodPred ("==", [ lit; AVar x ]) when String.equal x.x id.x -> Some lit
    | _ -> None
  in
  match is_eq xprop with
  | None -> (true, And [ xprop; prop ])
  | Some y -> (false, subst_id_with_lit prop id.x y)

let simp_exists eqv t =
  let is_eq = function
    | MethodPred ("==", [ AVar x; lit ]) when String.equal x.x eqv -> Some lit
    | MethodPred ("==", [ lit; AVar x ]) when String.equal x.x eqv -> Some lit
    | _ -> None
  in
  match t with
  | And es -> (
      let res = List.filter_map is_eq es in
      match res with
      | [] -> (true, t)
      | lit :: _ -> (false, subst_id_with_lit t eqv lit))
  | _ -> (true, t)

(* let rec simp_lit_lit t = *)
(*   match t with *)
(*   | ACint _ -> t *)
(*   | ACbool _ -> t *)
(*   | AVar _ -> t *)
(*   | AOp2 (op, a, b) -> ( *)
(*       match (op, simp_lit_lit a, simp_lit_lit b) with *)
(*       | "==", AVar id, AVar id' when String.equal id.x id'.x -> ACbool true *)
(*       | "==", ACint i, ACint i' -> ACbool (i == i') *)
(*       | "==", ACbool i, ACbool i' -> ACbool (i == i') *)
(*       | "==", a, b when lit_strict_eq a b -> ACbool true *)
(*       | op, a, b -> AOp2 (op, a, b)) *)

let simp_lit t =
  let rec aux t =
    match t with
    | Lit lit -> Lit lit
    | MethodPred ("==", [ AVar id; AVar id' ]) when String.equal id.x id'.x ->
        mk_true
    | MethodPred ("==", [ ACint i; ACint i' ]) -> Lit (ACbool (i == i'))
    | MethodPred ("==", [ ACbool i; ACbool i' ]) -> Lit (ACbool (i == i'))
    | MethodPred ("==", [ a; b ]) when lit_strict_eq a b -> mk_true
    | MethodPred (mp, args) -> MethodPred (mp, args)
    | Implies (e1, e2) -> Implies (aux e1, aux e2)
    | Ite (e1, e2, e3) -> Ite (aux e1, aux e2, aux e3)
    | Not e -> Not (aux e)
    | And es -> And (List.map aux es)
    | Or es -> Or (List.map aux es)
    | Iff (e1, e2) -> Iff (aux e1, aux e2)
    | Forall (u, e) -> Forall (u, aux e)
    | Exists (u, e) -> Exists (u, aux e)
  in
  aux t

let simp t = simp_conj_disj @@ simp_lit t

let to_uni_conjs t =
  let rec aux t =
    match t with And es -> List.concat @@ List.map aux es | _ -> [ t ]
  in
  aux t

let simp_exists_and qv xprop prop =
  let props = to_uni_conjs (And prop) in
  match xprop with
  | Lit (ACbool true) ->
      let if_keep, prop = simp_exists qv (And props) in
      let props = to_uni_conjs prop in
      if if_keep then (true, props) else (false, props)
  | _ -> (true, xprop :: props)

let lift_exists prop =
  let unique_add (eqvs, e) u =
    if List.exists (fun x -> String.equal u.x x.x) eqvs then
      let u' = { ty = u.ty; x = Unique.unique u.x } in
      (u' :: eqvs, subst_id e u.x u'.x)
    else (u :: eqvs, e)
  in
  (* let print l = *)
  (*   List.fold_left (fun str x -> Printf.sprintf "%s, %s" str x.x) "" l *)
  (* in *)
  let unique_merge2 eqvs1 (eqvs2, e2) =
    let eqvs, e2 = List.fold_left unique_add (eqvs1, e2) eqvs2 in
    (* let () = *)
    (*   Printf.printf "MERGE: [%s] ++ [%s] = [%s]\n" (print eqvs1) (print eqvs2) *)
    (*     (print eqvs) *)
    (* in *)
    (eqvs, e2)
  in
  let rec unique_merges = function
    | [] -> failwith "die"
    | [ (eqvs, e) ] -> (eqvs, [ e ])
    | h :: t ->
        let eqvs, es = unique_merges t in
        let eqvs, e = unique_merge2 eqvs h in
        (eqvs, e :: es)
  in
  let rec aux_e prop =
    match prop with
    | Lit _ | MethodPred (_, _) -> ([], prop)
    | Implies (e1, e2) ->
        let eqv1, e1 = aux_f e1 in
        let eqv2, e2 = aux_e e2 in
        let eqv, e2 = unique_merge2 eqv1 (eqv2, e2) in
        (eqv, Implies (e1, e2))
    | Ite (e1, e2, e3) ->
        let eqv1, e1 = aux_f e1 in
        let eqv2, e2 = aux_e e2 in
        let eqv3, e3 = aux_e e3 in
        let eqv, e2 = unique_merge2 eqv1 (eqv2, e2) in
        let eqv, e3 = unique_merge2 eqv (eqv3, e3) in
        (eqv, Ite (e1, e2, e3))
    | Not e ->
        let eqv, e = aux_f e in
        (eqv, Not e)
    | And es ->
        let eqvs, es = unique_merges @@ List.map aux_e es in
        (eqvs, And es)
    | Or es ->
        let eqvs, es = unique_merges @@ List.map aux_e es in
        (eqvs, Or es)
    | Iff (e1, e2) ->
        let e1 =
          match aux_e e1 with
          | [], e1 -> e1
          | _ -> failwith "not a existential quantified term"
        in
        let e2 =
          match aux_e e2 with
          | [], e2 -> e2
          | _ -> failwith "not a existential quantified term"
        in
        ([], Iff (e1, e2))
    | Forall (_, _) -> failwith "not a existential quantified term"
    | Exists (u, e) ->
        let eqvs, e = aux_e e in
        unique_add (eqvs, e) u
  and aux_f prop =
    match prop with
    | Lit _ | MethodPred (_, _) -> ([], prop)
    | Implies (e1, e2) ->
        let eqv1, e1 = aux_e e1 in
        let eqv2, e2 = aux_f e2 in
        let eqv, e2 = unique_merge2 eqv1 (eqv2, e2) in
        (eqv, Implies (e1, e2))
    | Ite (e1, e2, e3) ->
        let eqv1, e1 = aux_e e1 in
        let eqv2, e2 = aux_f e2 in
        let eqv3, e3 = aux_f e3 in
        let eqv, e2 = unique_merge2 eqv1 (eqv2, e2) in
        let eqv, e3 = unique_merge2 eqv (eqv3, e3) in
        (eqv, Ite (e1, e2, e3))
    | Not e ->
        let eqv, e = aux_e e in
        (eqv, Not e)
    | And es ->
        let eqvs, es = unique_merges @@ List.map aux_f es in
        (eqvs, And es)
    | Or es ->
        let eqvs, es = unique_merges @@ List.map aux_f es in
        (eqvs, Or es)
    | Iff (e1, e2) ->
        let e1 =
          match aux_f e1 with
          | [], e1 -> e1
          | _ -> failwith "not a existential quantified term"
        in
        let e2 =
          match aux_f e2 with
          | [], e2 -> e2
          | _ -> failwith "not a existential quantified term"
        in
        ([], Iff (e1, e2))
    | Exists (_, _) -> failwith "not a existential quantified term"
    | Forall (u, e) ->
        let eqvs, e = aux_f e in
        unique_add (eqvs, e) u
  in
  aux_e prop

type prefix = Uqv of string typed | Eqv of string typed
type ord = FE | EF

let flip (l, e) = (List.map (function Uqv a -> Eqv a | Eqv a -> Uqv a) l, e)
let prefix_to_string = function Uqv a -> a.x | Eqv a -> a.x

let to_pnf prop =
  let unique l (u, e) =
    if
      List.exists
        (String.equal (prefix_to_string u))
        (List.map prefix_to_string l)
    then
      match u with
      | Uqv u ->
          let u' = { ty = u.ty; x = Unique.unique u.x } in
          (Uqv u', subst_id e u.x u'.x)
      | Eqv u ->
          let u' = { ty = u.ty; x = Unique.unique u.x } in
          (Eqv u', subst_id e u.x u'.x)
    else (u, e)
  in
  let uniques l (us, e) =
    List.fold_left
      (fun (us, e) u ->
        let u, e = unique (l @ us) (u, e) in
        (us @ [ u ], e))
      ([], e) us
  in
  let unique_add l (u, e) =
    let u, e = unique l (u, e) in
    (l @ [ u ], e)
  in
  (* let print l = *)
  (*   List.fold_left (fun str x -> Printf.sprintf "%s, %s" str x.x) "" l *)
  (* in *)
  let rec aux_f = function
    | [], a -> a
    | a, [] -> a
    | Uqv a :: t1, t2 -> Uqv a :: aux_f (t1, t2)
    | t1, Uqv a :: t2 -> Uqv a :: aux_f (t1, t2)
    | t1, t2 -> aux_e (t1, t2)
  and aux_e = function
    | [], a -> a
    | a, [] -> a
    | Eqv a :: t1, t2 -> Eqv a :: aux_e (t1, t2)
    | t1, Eqv a :: t2 -> Eqv a :: aux_e (t1, t2)
    | t1, t2 -> aux_f (t1, t2)
  in
  let merge_prefix ord l1 l2 =
    match ord with FE -> aux_f (l1, l2) | EF -> aux_e (l1, l2)
  in
  (* let merge_prefix_from_ex l1 l2 = aux_e (l1, l2) in *)
  let unique_merge2 ord eqvs1 (eqvs2, e2) =
    let eqvs2, e2 = uniques eqvs1 (eqvs2, e2) in
    let eqvs = merge_prefix ord eqvs1 eqvs2 in
    (* let () = *)
    (*   Printf.printf "MERGE: [%s] ++ [%s] = [%s]\n" (print eqvs1) (print eqvs2) *)
    (*     (print eqvs) *)
    (* in *)
    (eqvs, e2)
  in
  let rec unique_merges ord = function
    | [] -> failwith "die"
    | [ (eqvs, e) ] -> (eqvs, [ e ])
    | h :: t ->
        let eqvs, es = unique_merges ord t in
        let eqvs, e = unique_merge2 ord eqvs h in
        (eqvs, e :: es)
  in
  let flip_ord = function FE -> EF | EF -> FE in
  let rec aux ord prop =
    match prop with
    | Lit _ | MethodPred (_, _) -> ([], prop)
    | Implies (e1, e2) ->
        let prefix1, e1 = flip @@ aux (flip_ord ord) e1 in
        let prefix2, e2 = aux ord e2 in
        let eqv, e2 = unique_merge2 ord prefix1 (prefix2, e2) in
        (eqv, Implies (e1, e2))
    | Ite (e1, e2, e3) ->
        let eqv1, e1 = flip @@ aux (flip_ord ord) e1 in
        let eqv2, e2 = aux ord e2 in
        let eqv3, e3 = aux ord e3 in
        let eqv, e2 = unique_merge2 ord eqv1 (eqv2, e2) in
        let eqv, e3 = unique_merge2 ord eqv (eqv3, e3) in
        (eqv, Ite (e1, e2, e3))
    | Not e ->
        let eqv, e = flip @@ aux (flip_ord ord) e in
        (eqv, Not e)
    | And es ->
        let eqvs, es = unique_merges ord @@ List.map (aux ord) es in
        (eqvs, And es)
    | Or es ->
        let eqvs, es =
          unique_merges ord
          @@ List.map (fun e -> flip @@ aux (flip_ord ord) e) es
        in
        (eqvs, Or es)
    | Iff (e1, e2) -> aux ord (And [ Implies (e1, e2); Implies (e2, e1) ])
    | Forall (u, e) ->
        let prefix, e = aux ord e in
        unique_add prefix (Uqv u, e)
    | Exists (u, e) ->
        let prefix, e = aux ord e in
        unique_add prefix (Eqv u, e)
  in
  aux FE prop

let to_fe_nf prop =
  let l, prop = to_pnf prop in
  let rec get_ex eqvs = function
    | [] -> eqvs
    | Eqv u :: t -> get_ex (eqvs @ [ u ]) t
    | Uqv _ :: _ -> failwith "not a EPR"
  in
  let rec get_fa uqvs = function
    | [] -> (uqvs, [])
    | Eqv u :: t -> (uqvs, get_ex [] (Eqv u :: t))
    | Uqv u :: t -> get_fa (uqvs @ [ u ]) t
  in
  let uqvs, eqvs = get_fa [] l in
  (uqvs, eqvs, prop)

let to_e_nf prop =
  let uqvs, eqvs, prop = to_fe_nf prop in
  match uqvs with [] -> (eqvs, prop) | _ -> failwith "not a eq form"

open Sugar

let assume_fe file line prop =
  let rec aux = function
    | Forall (u, e) ->
        let eqs, e = aux e in
        (u :: eqs, e)
    | e ->
        if has_qv e then _failatwith file line "not a forall quantified"
        else ([], e)
  in
  aux prop

let qv_only_in_dt_mp file line prop dt =
  let qvs, prop = assume_fe file line prop in
  let rec aux = function
    | Lit _ -> true
    | MethodPred (mp, args) when not (is_bop mp) ->
        let args =
          List.filter_map (function AVar x -> Some x | _ -> None) args
        in
        if
          List.exists
            (fun x -> List.exists (fun y -> String.equal x.x y.x) args)
            qvs
        then
          let args = List.filter (fun x -> is_dt x.ty) args in
          match args with
          | [ dt' ] when String.equal dt'.x dt.x -> String.equal dt'.x dt.x
          | _ -> false
        else true
    | MethodPred (_, _) -> true
    | Implies (e1, e2) -> aux e1 && aux e2
    | Ite (e1, e2, e3) -> aux e1 && aux e2 && aux e3
    | Not e -> aux e
    | And es -> List.for_all aux es
    | Or es -> List.for_all aux es
    | Iff (e1, e2) -> aux e1 && aux e2
    | Forall (_, _) | Exists (_, _) -> _failatwith file line "qv"
  in
  aux prop

let rename_destruct_uprop file line upre =
  let ids, prop = assume_fe file line upre in
  List.fold_right
    (fun id (ids', prop) ->
      let id' = map Rename.unique id in
      (id' :: ids', subst_id prop id.x id'.x))
    ids ([], prop)

let lift_qv_over_mp_in_uprop file line prop edts =
  let rec aux prop =
    match prop with
    | Lit _ | MethodPred (_, _) | Implies (_, _) | Not _ | Ite (_, _, _) | Iff _
      ->
        ([], prop)
    | And es ->
        let qvs, props = List.split @@ List.map aux es in
        (List.concat qvs, And props)
    | Or es ->
        let qvs, props = List.split @@ List.map aux es in
        (List.concat qvs, Or props)
    | Forall (_, _) ->
        let fv = fv prop in
        if List.exists (fun edt -> List.exists (String.equal edt.x) fv) edts
        then ([], prop)
        else rename_destruct_uprop file line prop
    | Exists (_, _) -> _failatwith file line (Frontend.layout prop)
    (* | _ -> _failatwith file line (Frontend.layout prop) *)
  in
  aux prop

let instantiate_uqvs_in_uprop_ file line prop choices =
  let uqvs, prop = assume_fe file line prop in
  let len = List.length uqvs in
  let props =
    List.map (fun uqvs' ->
        List.fold_right
          (fun (x, y) prop -> subst_id prop x.x y.x)
          (List.combine uqvs uqvs') prop)
    @@ List.choose_n choices len
  in
  match props with [] -> mk_true | _ -> And props

let instantiate_uqvs_in_uprop file line prop choices =
  let rec aux prop =
    match prop with
    | Lit _ | MethodPred (_, _) -> prop
    | Implies (e1, e2) -> Implies (aux e1, aux e2)
    | Ite (e1, e2, e3) -> Ite (aux e1, aux e2, aux e3)
    | Not e -> Not (aux e)
    | Iff (e1, e2) -> Iff (aux e1, aux e2)
    | And es -> And (List.map aux es)
    | Or es -> Or (List.map aux es)
    | Forall (_, _) -> instantiate_uqvs_in_uprop_ file line prop choices
    | Exists (_, _) ->
        _failatwith file line
          (spf "instantiate_uqvs_in_uprop: exists, %s" @@ Frontend.layout prop)
  in
  aux prop

let assume_tope_uprop file line prop =
  let rec aux = function
    | Exists (u, e) ->
        let eqs, e = aux e in
        (u :: eqs, e)
    | e ->
        if only_uq e then ([], e)
        else _failatwith file line (Frontend.layout prop)
  in
  aux prop

let tope_to_prop (eqvs, prop) =
  List.fold_right (fun eqv prop -> Exists (eqv, prop)) eqvs prop

let topu_to_prop (uqvs, prop) =
  List.fold_right (fun eqv prop -> Forall (eqv, prop)) uqvs prop

let disjunct_tope_uprop file line props =
  let eqvs, bodys =
    List.split @@ List.map (assume_tope_uprop file line) props
  in
  tope_to_prop (List.concat eqvs, Or bodys)

let conjunct_tope_uprop file line props =
  let eqvs, bodys =
    List.split @@ List.map (assume_tope_uprop file line) props
  in
  tope_to_prop (List.concat eqvs, And bodys)

let conjunct_base_to_tope_uprop_ (xeqvs, xprop) (eqvs1, prop1) =
  match xeqvs with
  | [] -> (eqvs1, And [ xprop; prop1 ])
  | [ id ] -> (
      let is_eq = function
        | MethodPred ("==", [ AVar x; lit ]) when String.equal x.x id.x ->
            Some lit
        | MethodPred ("==", [ lit; AVar x ]) when String.equal x.x id.x ->
            Some lit
        | _ -> None
      in
      match is_eq xprop with
      | None -> (id :: eqvs1, And [ xprop; prop1 ])
      | Some y -> (eqvs1, subst_id_with_lit prop1 id.x y))
  | _ -> (xeqvs @ eqvs1, And [ xprop; prop1 ])

let conjunct_base_to_tope_uprop file line xprop prop =
  let xeqvs, xprop = assume_tope_uprop file line xprop in
  let eqvs1, prop1 = assume_tope_uprop file line prop in
  conjunct_base_to_tope_uprop_ (xeqvs, xprop) (eqvs1, prop1)
