module T = struct
  open Sexplib.Std

  type ty = Smtty.T.t [@@deriving sexp]
  type 'a typed = { ty : ty; x : 'a } [@@deriving sexp]

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

  let is_op = function
    | "==" | "!=" | "<" | ">" | "<=" | ">=" | "+" | "-" -> true
    | _ -> false

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

  (* TODO: type check *)
  let lit_get_ty lit =
    let aux = function
      | ACint _ -> Smtty.T.Int
      | ACbool _ -> Smtty.T.Bool
      | AVar id -> id.ty
      | AOp2 (mp, _, _) -> (
          match mp with
          | "==" | "!=" | "<" | ">" | "<=" | ">=" -> Smtty.T.Bool
          | "+" | "-" -> Smtty.T.Int
          | _ -> failwith "lit_get_ty: unknown op")
    in
    aux lit

  let typed_id_eq x y =
    if String.equal x.x y.x then
      let () =
        if Smtty.T.smtty_eq (x.ty, y.ty) then ()
        else failwith "prop naming error"
      in
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

  let simp_exists eqv t =
    let is_eq = function
      | Lit (AOp2 ("==", AVar x, lit)) when String.equal x.x eqv -> Some lit
      | Lit (AOp2 ("==", lit, AVar x)) when String.equal x.x eqv -> Some lit
      | _ -> None
    in
    match t with
    | And es -> (
        let res = List.filter_map is_eq es in
        match res with
        | [] -> (true, t)
        | lit :: _ -> (false, subst_id_with_lit t eqv lit))
    | _ -> (true, t)

  let rec simp_lit_lit t =
    match t with
    | ACint _ -> t
    | ACbool _ -> t
    | AVar _ -> t
    | AOp2 (op, a, b) -> (
        match (op, simp_lit_lit a, simp_lit_lit b) with
        | "==", AVar id, AVar id' when String.equal id.x id'.x -> ACbool true
        | "==", ACint i, ACint i' -> ACbool (i == i')
        | "==", ACbool i, ACbool i' -> ACbool (i == i')
        | "==", a, b when lit_strict_eq a b -> ACbool true
        | op, a, b -> AOp2 (op, a, b))

  let simp_lit t =
    let rec aux t =
      match t with
      | Lit lit -> Lit (simp_lit_lit lit)
      | MethodPred (mp, args) -> MethodPred (mp, List.map simp_lit_lit args)
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

  let simp_exists_and qvs qv xprop prop =
    let props = to_uni_conjs (And prop) in
    match xprop with
    | Lit (ACbool true) ->
        let if_keep, prop = simp_exists (snd qv) (And props) in
        let props = to_uni_conjs prop in
        if if_keep then (qvs @ [ qv ], props) else (qvs, props)
    | _ -> (qvs @ [ qv ], xprop :: props)
end
