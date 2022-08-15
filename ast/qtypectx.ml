open Sugar
open Underty.T
module NT = Normalty.T

type ctx = bodyt Typectx.t qted

open Typectx

let unify ty ctx =
  let ty, ctx = unify_qv_to (fv bodyt_fv) ty ctx in
  (ty, ctx)

let subtract ctx ctx' =
  let rec aux eq = function
    | l, [] -> l
    | [], _ -> _failatwith __FILE__ __LINE__ ""
    | h1 :: t1, h2 :: t2 ->
        if eq h1 h2 then aux eq (t1, t2) else aux eq (t1, h2 :: t2)
  in
  {
    uqvs =
      aux
        (fun x y -> String.equal x.x y.x && NT.eq x.ty y.ty)
        (ctx.uqvs, ctx'.uqvs);
    eqvs =
      aux
        (fun x y -> String.equal x.x y.x && NT.eq x.ty y.ty)
        (ctx.eqvs, ctx'.eqvs);
    k = subtract ctx.k ctx'.k;
  }

let add_to_right ctx (ty, x) =
  { ctx with k = Typectx.add_to_right ctx.k (ty, x) }

let add_to_rights ctx l = List.fold_left add_to_right ctx l

let close_qv_by_diff ctx ctx' { uqvs; eqvs; k } =
  let ctx'' = subtract ctx ctx' in
  { uqvs = uqvs @ ctx''.uqvs; eqvs = eqvs @ ctx''.eqvs; k }

let hide_vars_in_ctx ctx vars ty =
  List.fold_right
    (fun id { uqvs; eqvs; k } ->
      let idty =
        match Typectx.get_opt ctx.k id with
        | None -> _failatwith __FILE__ __LINE__ ""
        | Some idty -> idty
      in
      { uqvs; eqvs; k = hide_quantify_variable_in_bodyt id idty k })
    vars ty

let empty : ctx = without_qv Typectx.empty

let fv f { uqvs; eqvs; k } =
  List.map (fun x -> x.x) uqvs @ List.map (fun x -> x.x) eqvs @ Typectx.fv f k
