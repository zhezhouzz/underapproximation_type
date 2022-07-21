module S = Languages.Termlang
module T = Languages.NormalAnormal
module Type = Languages.Normalty
open S

let get_tp e =
  match e.ty with
  | None ->
      Type.Ty_unit
      (* failwith *)
      (*   (Sugar.spf "Never happen: untyped %s in the term language expr" *)
      (*      (Frontend.Expr.layout e)) *)
  | Some ty -> ty

let id_get_tp e =
  match e.ty with
  | None ->
      Type.Ty_unit
      (* failwith *)
      (*   (Sugar.spf "Never happen: untyped %s in the term language expr" e.x) *)
  | Some ty -> ty

let id_trans (e : id opttyped) = T.{ ty = id_get_tp e; x = e.x }
let mk_t_term ty x = T.{ ty; x }

type cont = T.id T.typed -> T.term T.typed
type conts = T.id T.typed list -> T.term T.typed

let freshname () = Rename.unique "x"
let ret () x = T.{ ty = x.ty; x = Var x.x }

let make_let x e body =
  T.{ ty = body.ty; x = Let ([ T.{ ty = e.ty; x } ], e, body) }

let bind (cont : cont) e =
  let x = freshname () in
  let body = cont T.{ ty = e.ty; x } in
  make_let x e body

(* TODO: alpha renaming *)
let rec to_anormal (e : term opttyped) : T.term T.typed = cps (ret ()) e

and cps_multi (conts : conts) (es : term opttyped list) : T.term T.typed =
  let names = List.map (fun e -> T.{ ty = get_tp e; x = freshname () }) es in
  List.fold_left
    (fun body (lhs, rhs) -> make_let lhs.T.x (to_anormal rhs) body)
    (conts names) (List.combine names es)

and cps (cont : cont) (e : term opttyped) : T.term T.typed =
  let ety = get_tp e in
  match e.x with
  | Const v -> bind cont (mk_t_term ety (T.Const v))
  | Var id -> bind cont (mk_t_term ety (T.Var id))
  | Tu es -> cps_multi (fun names -> bind cont (mk_t_term ety @@ T.Tu names)) es
  | Lam (ty, x, body) ->
      bind cont (mk_t_term ety (T.Lam (T.{ ty; x }, to_anormal body)))
  | App (e, es) ->
      cps
        (fun f ->
          cps_multi
            (fun names -> bind cont (mk_t_term ety @@ T.App (f, names)))
            es)
        e
  | Let (false, lhs, rhs, body) ->
      mk_t_term ety
      @@ T.Let
           ( List.map (fun (ty, x) -> T.{ ty; x }) lhs,
             to_anormal rhs,
             cps cont body )
  | Let (true, [ (ty, x) ], rhs, body) ->
      let f = T.{ ty; x } in
      mk_t_term ety
      @@ T.Let ([ f ], mk_t_term ety @@ T.Fix (f, to_anormal rhs), cps cont body)
  | Let (true, _, _, _) -> failwith "invalid term lang"
  | Ite (e1, e2, e3) ->
      cps (fun x -> mk_t_term ety @@ T.Ite (x, to_anormal e2, to_anormal e3)) e1
  | Match (e, cases) ->
      cps
        (fun x ->
          mk_t_term ety
          @@ T.Match
               ( x,
                 List.map
                   (fun case ->
                     {
                       T.constructor = case.constructor;
                       T.args = case.args;
                       T.exp = to_anormal case.exp;
                     })
                   cases ))
        e

let to_anormal_with_name x if_rec (e : term opttyped) : T.term T.typed =
  let open T in
  let e = to_anormal e in
  if if_rec then { ty = e.ty; x = Fix ({ ty = e.ty; x }, e) } else e

let id_trans_rev (e : T.id T.typed) = { ty = Some e.T.ty; x = e.T.x }

let rec to_term e =
  let to_var id = T.{ ty = id.T.ty; x = T.Var id.T.x } in
  let mk_s_term x = { ty = Some e.T.ty; x } in
  match e.T.x with
  | T.Const v -> mk_s_term (Const v)
  | T.Var id -> mk_s_term (Var id)
  | T.Tu es -> mk_s_term (Tu (List.map to_term @@ List.map to_var es))
  | T.Lam (x, body) -> mk_s_term (Lam (x.ty, x.x, to_term body))
  | T.Fix _ -> failwith "never happend fix"
  | T.App (e, es) ->
      mk_s_term
      @@ App (to_term @@ to_var e, List.map to_term @@ List.map to_var es)
  | T.Let ([ f ], T.{ x = T.Fix (_, rhs); _ }, body) ->
      (* let () = Printf.printf "failwith find f\n" in *)
      mk_s_term @@ Let (true, [ (f.ty, f.x) ], to_term rhs, to_term body)
  | T.Let (lhs, rhs, body) ->
      (* let () = *)
      (*   Printf.printf "let lhs = %s\n" *)
      (*     (Zzdatatype.Datatype.StrList.to_string *)
      (*     @@ List.map (fun x -> x.T.x) lhs) *)
      (* in *)
      mk_s_term
      @@ Let
           ( false,
             List.map (fun x -> T.(x.ty, x.x)) lhs,
             to_term rhs,
             to_term body )
  | T.Ite (e1, e2, e3) ->
      mk_s_term @@ Ite (to_term @@ to_var e1, to_term e2, to_term e3)
  | T.Match (x, cases) ->
      mk_s_term
      @@ Match
           ( to_term @@ to_var x,
             List.map
               (fun case ->
                 {
                   constructor = case.T.constructor;
                   args = case.T.args;
                   exp = to_term case.T.exp;
                 })
               cases )
