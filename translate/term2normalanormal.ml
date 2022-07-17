module S = Languages.Termlang
module T = Languages.NormalAnormal
open S

let id_trans (e : id typed) = T.{ ty = e.ty; x = e.x }
let mk_t_term ty x = T.{ ty; x }

type cont = T.id T.typed -> T.term T.typed
type conts = T.id T.typed list -> T.term T.typed

let freshname () = T.{ ty = None; x = Rename.unique "x" }
let ret () x = T.{ ty = x.ty; x = Var x.x }

let bind cont e =
  let x = freshname () in
  T.make_untyped @@ T.Let ([ x ], e, cont x)

let rec to_anormal (e : term typed) : T.term T.typed = cps (ret ()) e

and cps_multi (conts : conts) (es : term typed list) : T.term T.typed =
  let names = List.map (fun _ -> freshname ()) es in
  List.fold_left
    (fun body (lhs, rhs) ->
      T.make_untyped @@ T.Let ([ lhs ], to_anormal rhs, body))
    (conts names) (List.combine names es)

and cps (cont : cont) (e : term typed) : T.term T.typed =
  match e.x with
  | Const v -> bind cont (mk_t_term e.ty (T.Const v))
  | Var id -> bind cont (mk_t_term e.ty (T.Var id))
  | Tu es ->
      cps_multi (fun names -> bind cont (T.make_untyped @@ T.Tu names)) es
  | Lam (xs, body) ->
      bind cont (mk_t_term e.ty (T.Lam (List.map id_trans xs, to_anormal body)))
  | App (e, es) ->
      cps
        (fun f ->
          cps_multi
            (fun names -> bind cont (T.make_untyped @@ T.App (f, names)))
            es)
        e
  | Let (false, lhs, rhs, body) ->
      T.make_untyped
      @@ T.Let (List.map id_trans lhs, to_anormal rhs, cps cont body)
  | Let (true, [ f ], rhs, body) ->
      let f = id_trans f in
      T.make_untyped
      @@ T.Let
           ([ f ], T.make_untyped @@ T.Fix (f, to_anormal rhs), cps cont body)
  | Let (true, _, _, _) -> failwith "invalid term lang"
  | Ite (e1, e2, e3) ->
      cps
        (fun x -> T.make_untyped @@ T.Ite (x, to_anormal e2, to_anormal e3))
        e1
  | Match (e, cases) ->
      cps
        (fun x ->
          T.make_untyped
          @@ T.Match
               ( x,
                 List.map
                   (fun case ->
                     {
                       T.constuctor = case.constuctor;
                       T.args = case.args;
                       T.exp = to_anormal case.exp;
                     })
                   cases ))
        e

let id_trans_rev (e : T.id T.typed) = { ty = e.T.ty; x = e.T.x }

let rec to_term e =
  let to_var id = T.{ ty = id.T.ty; x = T.Var id.T.x } in
  let mk_s_term x = { ty = e.T.ty; x } in
  match e.T.x with
  | T.Const v -> mk_s_term (Const v)
  | T.Var id -> mk_s_term (Var id)
  | T.Tu es -> mk_s_term (Tu (List.map to_term @@ List.map to_var es))
  | T.Lam (xs, body) -> mk_s_term (Lam (List.map id_trans_rev xs, to_term body))
  | T.Fix _ -> failwith "never happend fix"
  | T.App (e, es) ->
      mk_s_term
      @@ App (to_term @@ to_var e, List.map to_term @@ List.map to_var es)
  | T.Let ([ f ], T.{ x = T.Fix (_, rhs); _ }, body) ->
      (* let () = Printf.printf "failwith find f\n" in *)
      mk_s_term
      @@ Let (true, List.map id_trans_rev [ f ], to_term rhs, to_term body)
  | T.Let (lhs, rhs, body) ->
      (* let () = *)
      (*   Printf.printf "let lhs = %s\n" *)
      (*     (Zzdatatype.Datatype.StrList.to_string *)
      (*     @@ List.map (fun x -> x.T.x) lhs) *)
      (* in *)
      mk_s_term
      @@ Let (false, List.map id_trans_rev lhs, to_term rhs, to_term body)
  | T.Ite (e1, e2, e3) ->
      mk_s_term @@ Ite (to_term @@ to_var e1, to_term e2, to_term e3)
  | T.Match (x, cases) ->
      mk_s_term
      @@ Match
           ( to_term @@ to_var x,
             List.map
               (fun case ->
                 {
                   constuctor = case.T.constuctor;
                   args = case.T.args;
                   exp = to_term case.T.exp;
                 })
               cases )
