module S = Languages.Termlang
module T = Languages.NormalAnormal
module Type = Languages.Normalty
open S
open Sugar

let get_tp e =
  match e.ty with
  | None ->
      (* Type.Ty_unit *)
      _failatwith __FILE__ __LINE__
        (spf "Never happen: untyped %s in the term language expr"
           (Frontend.Expr.layout e))
  | Some ty -> ty

let id_get_tp e =
  match e.ty with
  | None ->
      (* Type.Ty_unit *)
      _failatwith __FILE__ __LINE__
        (spf "Never happen: untyped %s in the term language expr" e.x)
  | Some ty -> ty

let id_trans (e : id opttyped) = T.{ ty = id_get_tp e; x = e.x }
let mk_t_term ty x = T.{ ty; x }

type cont = T.value T.typed -> T.term T.typed
type conts = T.id T.typed list -> T.term T.typed

let freshname () = Rename.unique "x"
let ret () x = T.{ ty = x.ty; x = V x.x }
let force_naming name = match name with None -> freshname () | Some x -> x

(* let bind_value cont lit = *)
(*   let open T in *)
(*   match lit.x with *)
(*   | Var id -> cont { ty = value.ty; x = lit } *)
(*   | _ -> *)
(*       let x = freshname () in *)
(*       let body = cont { ty = value.ty; x = Var x } in *)
(*       make_letval x value body *)

let typed_lit_to_id file line f =
  let open T in
  match f.x with
  | Var x -> { ty = f.ty; x }
  | _ -> _failatwith file line "should not happen"

let id_cont_to_value_cont cont value =
  let open T in
  match value.x with
  | Lit (Var id) -> cont { ty = value.ty; x = id }
  | _ ->
      let x = freshname () in
      let body = cont { ty = value.ty; x } in
      make_letval x value body

let lit_cont_to_value_cont cont value =
  let open T in
  match value.x with
  | Lit lit -> cont { ty = value.ty; x = lit }
  | _ ->
      let x = freshname () in
      let body = cont { ty = value.ty; x = Var x } in
      make_letval x value body

(* TODO: alpha renaming *)
let rec convert (cont : cont) (e : term opttyped) (ename : string option) :
    T.term T.typed =
  let ety = get_tp e in
  let open T in
  match e.x with
  | Const v ->
      let lit =
        match v with
        | I i -> ConstI i
        | B b -> ConstB b
        | _ -> _failatwith __FILE__ __LINE__ "unimp complex const"
      in
      cont { ty = ety; x = Lit lit }
  | Var id -> (
      match ename with
      | None -> cont T.{ ty = ety; x = Lit (Var id) }
      | Some ename ->
          let value = T.{ ty = ety; x = Lit (Var id) } in
          T.make_letval ename value (cont value))
  | Tu [] -> _failatwith __FILE__ __LINE__ "wrong tuple"
  | Tu [ e ] -> convert cont e ename
  | Tu es ->
      convert_multi
        T.(
          fun args ->
            let tu = { ty = ety; x = force_naming ename } in
            let body = cont @@ id_to_value tu in
            { ty = body.ty; x = LetTu { tu; args; body } })
        es
  | Lam (ty, x, body) ->
      cont { ty = ety; x = Lam ({ ty; x }, to_anormal body None) }
  (* NOTE: zero arguments applicaition is still need a name *)
  (* | App (e, []) -> convert cont e target_name *)
  | Op (op, es) ->
      convert_multi
        (fun args ->
          let ret = { ty = ety; x = force_naming ename } in
          let body = cont @@ id_to_value ret in
          { ty = body.ty; x = LetOp { ret; op; args; body } })
        es
  | App (e, es) ->
      convert
        ( id_cont_to_value_cont @@ fun f ->
          convert_multi
            (fun args ->
              let ret = { ty = ety; x = force_naming ename } in
              let body = cont @@ id_to_value ret in
              { ty = body.ty; x = LetApp { ret; f; args; body } })
            es )
        e None
  | Let (false, lhs, rhs, body) -> (
      let body = convert cont body ename in
      let lhs = List.map T.(fun (ty, x) -> { ty; x }) lhs in
      match lhs with
      | [] -> _failatwith __FILE__ __LINE__ ""
      | [ lhs ] -> convert (fun _ -> body) rhs (Some lhs.x)
      | _ ->
          convert
            ( id_cont_to_value_cont @@ fun tu ->
              { ty = body.ty; x = LetDeTu { args = lhs; tu; body } } )
            rhs None)
  | Let (true, [ (ty, x) ], rhs, body) ->
      let body = convert cont body ename in
      let f = T.{ ty; x } in
      convert
        (fun rhs ->
          let rhs = { ty = f.ty; x = Fix (f, rhs) } in
          make_letval f.x rhs body)
        rhs None
  | Let (true, _, _, _) -> _failatwith __FILE__ __LINE__ "invalid term lang"
  | Ite (e1, e2, e3) ->
      convert
        ( id_cont_to_value_cont @@ fun cond ->
          let e_t = convert cont e2 ename in
          let e_f = convert cont e3 ename in
          mk_t_term ety @@ Ite { cond; e_t; e_f } )
        e1 None
  | Match (e, cases) ->
      convert
        ( id_cont_to_value_cont @@ fun matched ->
          let cases =
            List.map
              (fun (case : S.case) ->
                let constructor_ty =
                  match case.S.constructor.ty with
                  | None -> _failatwith __FILE__ __LINE__ "die"
                  | Some ty -> ty
                in
                {
                  constructor =
                    { ty = constructor_ty; x = case.S.constructor.x };
                  args = case.S.args;
                  exp = convert cont case.S.exp ename;
                })
              cases
          in
          mk_t_term ety @@ Match { matched; cases } )
        e None

and to_anormal (e : term opttyped) ename : T.term T.typed =
  convert (ret ()) e ename

and convert_multi (conts : conts) (es : term opttyped list) : T.term T.typed =
  let open T in
  (List.fold_left
     (fun (conts : conts) rhs vs ->
       convert
         (fun v ->
           let x = freshname () in
           let body = conts ({ ty = v.ty; x } :: vs) in
           make_letval x v body)
         rhs None)
     conts es)
    []

let id_trans_rev (e : T.id T.typed) = { ty = Some e.T.ty; x = e.T.x }

let to_term e =
  let to_var id = { ty = Some id.T.ty; x = Var id.T.x } in
  let to_tid id = (id.T.ty, id.T.x) in
  let aux_lit x =
    match x with
    | T.ConstI i -> Const (Value.I i)
    | T.ConstB b -> Const (Value.B b)
    | T.Var id -> Var id
  in
  (* let lit_to_var lit = { ty = Some lit.T.ty; x = aux_lit lit.x } in *)
  let rec aux_value e =
    let x =
      match e.T.x with
      | T.Lit lit -> aux_lit lit
      | T.Lam (x, body) -> Lam (x.ty, x.x, aux body)
      | T.Fix (_, body) -> (aux_value body).x
    in
    { ty = Some e.T.ty; x }
  and aux e =
    let x =
      match e.T.x with
      | V x -> (aux_value T.{ ty = e.ty; x }).x
      | T.LetTu { tu; args; body } ->
          Let
            ( false,
              [ to_tid tu ],
              { ty = Some tu.T.ty; x = Tu (List.map to_var args) },
              aux body )
      | T.LetDeTu { tu; args; body } ->
          Let (false, List.map to_tid args, to_var tu, aux body)
      | T.LetApp { ret; f; args; body } ->
          Let
            ( false,
              [ to_tid ret ],
              { ty = Some ret.T.ty; x = App (to_var f, List.map to_var args) },
              aux body )
      | T.LetOp { ret; op; args; body } ->
          Let
            ( false,
              [ to_tid ret ],
              {
                ty = Some ret.T.ty;
                x =
                  App
                    ( { ty = None; x = Var (Languages.Op.op_to_string op) },
                      List.map to_var args );
              },
              aux body )
      | T.LetVal { lhs; rhs; body } ->
          Let (false, [ to_tid lhs ], aux_value rhs, aux body)
      | T.Ite { cond; e_t; e_f } -> Ite (to_var cond, aux e_t, aux e_f)
      | T.Match { matched; cases } ->
          Match
            ( to_var matched,
              List.map
                (fun case ->
                  {
                    constructor =
                      {
                        ty = Some case.T.constructor.ty;
                        x = case.T.constructor.x;
                      };
                    args = case.T.args;
                    exp = aux case.T.exp;
                  })
                cases )
    in
    { ty = Some e.T.ty; x }
  in
  aux e
