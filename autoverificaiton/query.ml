open Prop
open Z3aux
open Z3
open Sugar

let make_forall ctx qv body =
  if List.length qv == 0 then body
  else
    Quantifier.expr_of_quantifier
      (Quantifier.mk_forall_const ctx qv body (Some 1) [] [] None None)

let make_exists ctx qv body =
  if List.length qv == 0 then body
  else
    Quantifier.expr_of_quantifier
      (Quantifier.mk_exists_const ctx qv body (Some 1) [] [] None None)

let z3func ctx funcname inptps outtp =
  FuncDecl.mk_func_decl ctx
    (Symbol.mk_string ctx funcname)
    (List.map (tp_to_sort ctx) inptps)
    (tp_to_sort ctx outtp)

let lit_to_z3 ctx lit =
  let rec aux = function
    | ACint n -> int_to_z3 ctx n
    | ACbool n -> bool_to_z3 ctx n
    | AVar x -> tpedvar_to_z3 ctx (x.ty, x.x)
    | AOp2 (mp, a, b) -> (
        let a = aux a in
        let b = aux b in
        match mp with
        | "+" -> Z3.Arithmetic.mk_add ctx [ a; b ]
        | "-" -> Z3.Arithmetic.mk_sub ctx [ a; b ]
        | "*" -> Z3.Arithmetic.mk_mul ctx [ a; b ]
        | "/" -> Z3.Arithmetic.mk_div ctx a b
        | _ -> failwith @@ spf "unknown operator: %s" mp)
  in
  aux lit

type tmp =
  | TImplies0
  | TImplies1 of Expr.expr
  | TIte0
  | TIte1 of Expr.expr
  | TIte2 of Expr.expr * Expr.expr
  | TNot
  | TIff0
  | TIff1 of Expr.expr
  | TForall of Expr.expr
  | TExists of Expr.expr
  | TStop of Expr.expr

let machine ctx expr = function
  | TImplies0 -> TImplies1 expr
  | TIte0 -> TIte1 expr
  | TIte1 e1 -> TIte2 (e1, expr)
  | TIff0 -> TIff1 expr
  | TImplies1 e1 -> TStop (Z3.Boolean.mk_implies ctx e1 expr)
  | TIte2 (e1, e2) -> TStop (Z3.Boolean.mk_ite ctx e1 e2 expr)
  | TNot -> TStop (Z3.Boolean.mk_not ctx expr)
  | TIff1 e1 -> TStop (Z3.Boolean.mk_iff ctx e1 expr)
  | TForall u -> TStop (make_forall ctx [ u ] expr)
  | TExists u -> TStop (make_exists ctx [ u ] expr)
  | TStop _ -> _failatwith __FILE__ __LINE__ ""

let known_mp =
  [
    "hd";
    "mem";
    "ord";
    "len";
    "left";
    "right";
    "para";
    "sorted";
    "numblack";
    "noredred";
    "hdcolor";
    "complete";
    "rng";
    "heap";
    "rank";
  ]

let to_z3_ ctx = function
  | Lit lit -> lit_to_z3 ctx lit
  | MethodPred ("==", [ a; b ]) ->
      Z3.Boolean.mk_eq ctx (lit_to_z3 ctx a) (lit_to_z3 ctx b)
  | MethodPred ("==", _) -> _failatwith __FILE__ __LINE__ ""
  | MethodPred ("!=", [ a; b ]) ->
      Z3.Boolean.mk_not ctx
      @@ Z3.Boolean.mk_eq ctx (lit_to_z3 ctx a) (lit_to_z3 ctx b)
  | MethodPred ("!=", _) -> _failatwith __FILE__ __LINE__ ""
  | MethodPred ("<=", [ a; b ]) ->
      Z3.Arithmetic.mk_le ctx (lit_to_z3 ctx a) (lit_to_z3 ctx b)
  | MethodPred ("<=", _) -> _failatwith __FILE__ __LINE__ ""
  | MethodPred (">=", [ a; b ]) ->
      Z3.Arithmetic.mk_ge ctx (lit_to_z3 ctx a) (lit_to_z3 ctx b)
  | MethodPred (">=", _) -> _failatwith __FILE__ __LINE__ ""
  | MethodPred ("<", [ a; b ]) ->
      Z3.Arithmetic.mk_lt ctx (lit_to_z3 ctx a) (lit_to_z3 ctx b)
  | MethodPred ("<", _) -> _failatwith __FILE__ __LINE__ ""
  | MethodPred (">", [ a; b ]) ->
      Z3.Arithmetic.mk_gt ctx (lit_to_z3 ctx a) (lit_to_z3 ctx b)
  | MethodPred (">", _) -> _failatwith __FILE__ __LINE__ ""
  | MethodPred (mp, args) ->
      let () =
        if List.exists (String.equal mp) known_mp then ()
        else failwith (spf "unknown mp: %s" mp)
      in
      (* let () = Printf.printf "mp >>> %s\n" mp in *)
      let argsty = List.map lit_get_ty args in
      let args = List.map (lit_to_z3 ctx) args in
      let func = z3func ctx mp argsty Ty_bool in
      Z3.FuncDecl.apply func args
  | _ -> _failatwith __FILE__ __LINE__ ""

let to_z3 ctx prop =
  let rec aux prop =
    match prop with
    | Implies (p1, p2) -> Z3.Boolean.mk_implies ctx (aux p1) (aux p2)
    | Ite (p1, p2, p3) -> Z3.Boolean.mk_ite ctx (aux p1) (aux p2) (aux p3)
    | Not p -> Z3.Boolean.mk_not ctx (aux p)
    | And ps -> Z3.Boolean.mk_and ctx (List.map aux ps)
    | Or ps -> Z3.Boolean.mk_or ctx (List.map aux ps)
    | Iff (p1, p2) -> Z3.Boolean.mk_iff ctx (aux p1) (aux p2)
    | Forall (u, body) ->
        make_forall ctx [ tpedvar_to_z3 ctx (u.ty, u.x) ] (aux body)
    | Exists (u, body) ->
        make_exists ctx [ tpedvar_to_z3 ctx (u.ty, u.x) ] (aux body)
    | Lit _ | MethodPred _ -> to_z3_ ctx prop
  in
  aux prop

let to_z3_tail ctx prop =
  let rec aux c prop =
    match prop with
    | Implies (p1, p2) ->
        aux
          (fun p2 -> aux (fun p1 -> c (Z3.Boolean.mk_implies ctx p1 p2)) p1)
          p2
    | Ite (p1, p2, p3) ->
        aux
          (fun p3 ->
            aux
              (fun p2 -> aux (fun p1 -> c (Z3.Boolean.mk_ite ctx p1 p2 p3)) p1)
              p2)
          p3
    | Not p -> aux (fun p -> c (Z3.Boolean.mk_not ctx p)) p
    | And ps -> aux_multi (fun ps -> c (Z3.Boolean.mk_and ctx ps)) ps
    | Or ps -> aux_multi (fun ps -> c (Z3.Boolean.mk_or ctx ps)) ps
    | Iff (p1, p2) ->
        aux (fun p2 -> aux (fun p1 -> c (Z3.Boolean.mk_iff ctx p1 p2)) p1) p2
    | Forall (u, body) ->
        aux
          (fun body ->
            c (make_forall ctx [ tpedvar_to_z3 ctx (u.ty, u.x) ] body))
          body
    | Exists (u, body) ->
        aux
          (fun body ->
            c (make_exists ctx [ tpedvar_to_z3 ctx (u.ty, u.x) ] body))
          body
    | Lit _ | MethodPred _ -> c (to_z3_ ctx prop)
  and aux_multi cs props =
    match props with
    | [] -> cs []
    | h :: t -> aux (fun h -> aux_multi (fun t -> cs (h :: t)) t) h
  in
  aux (fun p -> p) prop
