open Languagez
open Normal_lit_typing
open Sugar

type t = Nt.t

let bi_typed_prop_check (ctx : t ctx) (prop : t option prop) : t prop =
  let rec aux ctx prop =
    match prop with
    | Lit lit -> Lit (bi_typed_lit_check ctx lit Nt.bool_ty)
    | Implies (e1, e2) -> Implies (aux ctx e1, aux ctx e2)
    | Ite (e1, e2, e3) -> Ite (aux ctx e1, aux ctx e2, aux ctx e3)
    | Not e -> Not (aux ctx e)
    | And es -> And (List.map (aux ctx) es)
    | Or es -> Or (List.map (aux ctx) es)
    | Iff (e1, e2) -> Iff (aux ctx e1, aux ctx e2)
    | MethodPred { mpred; args } ->
        let mpred_ty =
          match Typectx.get_opt ctx mpred with
          | None ->
              _failatwith __FILE__ __LINE__ (spf "prop: cannot find %s" mpred)
          | Some ty -> ty
        in
        let args' = List.map (bi_typed_lit_infer ctx) args in
        let mpred_ty =
          Nt._type_unify __FILE__ __LINE__ mpred_ty
            (Nt.construct_arr_tp (List.map _get_ty args', Nt.Ty_bool))
        in
        let argsty, _ = Nt.destruct_arr_tp mpred_ty in
        let args =
          List.map (fun (x, ty) -> bi_typed_lit_check ctx x ty)
          @@ _safe_combine __FILE__ __LINE__ args argsty
        in
        MethodPred { mpred; args }
    | Forall { qv; body } ->
        let qv = __force_typed __FILE__ __LINE__ qv in
        Forall { qv; body = aux (add_to_right ctx qv) body }
    | Exists { qv; body } ->
        let qv = __force_typed __FILE__ __LINE__ qv in
        Exists { qv; body = aux (add_to_right ctx qv) body }
  in
  aux ctx prop
