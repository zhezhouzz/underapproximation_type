open Ast
module S = Termlang
module T = NL
module Type = NT
module Typed = NNtyped
open S
open Sugar
open Abstraction

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

open Typed

let id_trans (e : id opttyped) = { ty = id_get_tp e; x = e.x }
let mk_t_term ty x = { ty; x }

type cont = T.value typed -> T.term typed
type conts = T.value typed list -> T.term typed

let freshname () = Rename.unique "x"
let ret_cont : cont = fun v -> T.{ ty = v.ty; x = V v }
let force_naming name = match name with None -> freshname () | Some x -> x

let typed_lit_to_id file line f =
  let open T in
  match f.x with
  | Var x -> { ty = f.ty; x }
  | _ -> _failatwith file line "should not happen"

let value_to_id v =
  let open T in
  match v.x with
  | Var x -> x
  | _ -> _failatwith __FILE__ __LINE__ "never happen"

(* let id_cont_to_value_cont cont value = *)
(*   let open T in *)
(*   match value.x with *)
(*   | Lit (Var id) -> cont { ty = value.ty; x = id } *)
(*   | _ -> *)
(*       let x = freshname () in *)
(*       let body = cont { ty = value.ty; x } in *)
(*       make_letval x value body *)

(* let lit_cont_to_value_cont cont value = *)
(*   let open T in *)
(*   match value.x with *)
(*   | Lit lit -> cont { ty = value.ty; x = lit } *)
(*   | _ -> *)
(*       let x = freshname () in *)
(*       let body = cont { ty = value.ty; x = Var x } in *)
(*       make_letval x value body *)

let to_term e =
  let open S in
  let to_var id = S.{ ty = Some id.T.ty; x = Var id.x } in
  let to_tid id = (id.T.ty, id.T.x) in
  let aux_lit x =
    match x with
    | T.ConstI i -> Const (Value.I i)
    | T.ConstB b -> Const (Value.B b)
  in
  (* let lit_to_var lit = { ty = Some lit.ty; x = aux_lit lit.x } in *)
  let rec aux_value (e : T.value typed) : term opttyped =
    let x =
      match e.x with
      | T.Var id -> Var id.x
      | T.Exn -> Exn
      | T.Lit lit -> aux_lit lit
      | T.Lam { lamarg; lambody } -> Lam (lamarg.ty, lamarg.x, None, aux lambody)
      | T.Fix { fstarg; lambody; _ } ->
          (aux_value { x = T.Lam { lamarg = fstarg; lambody }; ty = e.ty }).x
    in
    { ty = Some e.ty; x }
  and aux e =
    let x =
      match e.x with
      | V v -> (aux_value v).x
      | T.LetTu { tu; args; body } ->
          Let
            ( false,
              [ to_tid tu ],
              { ty = Some tu.ty; x = Tu (List.map aux_value args) },
              aux body )
      | T.LetDeTu { tu; args; body } ->
          Let (false, List.map to_tid args, aux_value tu, aux body)
      | T.LetApp { ret; f; args; body } ->
          Let
            ( false,
              [ to_tid ret ],
              { ty = Some ret.ty; x = App (to_var f, List.map aux_value args) },
              aux body )
      | T.LetDtConstructor { ret; f; args; body } ->
          let fty = T.recover_dt_constructor_ty (ret, args) in
          Let
            ( false,
              [ to_tid ret ],
              {
                ty = Some ret.ty;
                x =
                  App (S.{ ty = Some fty; x = Var f.x }, List.map aux_value args);
              },
              aux body )
      | T.LetOp { ret; op; args; body } ->
          Let
            ( false,
              [ to_tid ret ],
              {
                ty = Some ret.ty;
                x =
                  App
                    ( { ty = None; x = Var (Op.op_to_string op) },
                      List.map aux_value args );
              },
              aux body )
      | T.LetVal { lhs; rhs; body } ->
          Let (false, [ to_tid lhs ], aux_value rhs, aux body)
      | T.Ite { cond; e_t; e_f } -> Ite (aux_value cond, aux e_t, aux e_f)
      | T.Match { matched; cases } ->
          Match
            ( aux_value matched,
              List.map
                (fun case ->
                  {
                    constructor =
                      {
                        ty = Some case.T.constructor.ty;
                        x = case.T.constructor.x;
                      };
                    args = List.map (fun x -> x.T.x) case.T.args;
                    exp = aux case.T.exp;
                  })
                cases )
    in
    { ty = Some e.ty; x }
  in
  aux e

(* TODO: alpha renaming *)
let rec convert (cont : cont) (e : term opttyped) (ename : string option) :
    T.term typed =
  let ety = get_tp e in
  let open T in
  (* let () = Printf.printf "W: %s\n" @@ Frontend.Expr.layout e in *)
  match e.x with
  | Exn -> (
      (* HACK: as var *)
      match ename with
      | None -> cont @@ id_to_value { ty = ety; x = "Exn" }
      | Some ename ->
          _failatwith __FILE__ __LINE__
            (spf "To Anormal, Handel Exn (%s)" ename))
  | Const v ->
      let lit =
        match v with
        | I i -> ConstI i
        | B b -> ConstB b
        | _ -> _failatwith __FILE__ __LINE__ "unimp complex const"
      in
      let lit = { ty = ety; x = Lit lit } in
      let res =
        match ename with
        | None -> cont lit
        | Some ename -> T.make_letval ename lit (cont lit)
      in
      (* let () = *)
      (*   Printf.printf "res ::\n%s\n" (Frontend.Expr.layout (to_term res)) *)
      (* in *)
      res
  | Var id -> (
      let vid = id_to_value { ty = ety; x = id } in
      match ename with
      | None -> cont vid
      | Some ename -> T.make_letval ename vid (cont vid))
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
  | Lam (ty, x, _, body) ->
      let rhs =
        {
          ty = ety;
          x = Lam { lamarg = { ty; x }; lambody = to_anormal body None };
        }
      in
      (* NOTE: We always assign a name to a function *)
      let lhs = { ty = ety; x = force_naming ename } in
      T.make_letval lhs.x rhs (cont @@ id_to_value lhs)
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
        (fun v ->
          let f = value_to_id v in
          try
            let _ = Prim.get_primitive_normal_ty f.x in
            (* For the data constructor with 0 argument, treat as variable *)
            match es with
            (* | [] -> cont v *)
            | _ ->
                convert_multi
                  (fun args ->
                    let ret = { ty = ety; x = force_naming ename } in
                    let body = cont @@ id_to_value ret in
                    {
                      ty = body.ty;
                      x = LetDtConstructor { ret; f; args; body };
                    })
                  es
          with _ ->
            convert_multi
              (fun args ->
                let ret = { ty = ety; x = force_naming ename } in
                let body = cont @@ id_to_value ret in
                { ty = body.ty; x = LetApp { ret; f; args; body } })
              es)
        e None
  | Let (false, lhs, rhs, body) -> (
      let body = convert cont body ename in
      let lhs = List.map (fun (ty, x) -> { ty; x }) lhs in
      match lhs with
      | [] -> _failatwith __FILE__ __LINE__ ""
      | [ lhs ] -> convert (fun _ -> body) rhs (Some lhs.x)
      | _ ->
          convert
            (fun tu -> { ty = body.ty; x = LetDeTu { args = lhs; tu; body } })
            rhs None)
  | Let (true, [ (ty, x) ], rhs, body) ->
      let body = convert cont body ename in
      let fixname = { ty; x } in
      let func = convert ret_cont rhs None in
      let rhs =
        match func.x with
        | V { x = Lam { lamarg; lambody }; _ } ->
            { x = Fix { fixname; fstarg = lamarg; lambody }; ty }
        | _ -> _failatwith __FILE__ __LINE__ "never happen"
      in
      make_letval fixname.x rhs body
  | Let (true, _, _, _) -> _failatwith __FILE__ __LINE__ "invalid term lang"
  | Ite (e1, e2, e3) ->
      convert
        (fun cond ->
          let e_t = convert cont e2 ename in
          let e_f = convert cont e3 ename in
          mk_t_term ety @@ Ite { cond; e_t; e_f })
        e1 None
  | Match (e, cases) ->
      convert
        (fun matched ->
          let cases =
            List.map
              (fun (case : S.case) ->
                let constructor_ty =
                  match case.S.constructor.ty with
                  | None -> _failatwith __FILE__ __LINE__ "die"
                  | Some ty -> ty
                in
                (* let _ = *)
                (*   Printf.printf "zz: (snd constructor_ty): %s\n" *)
                (*     (Type.layout (snd constructor_ty)) *)
                (* in *)
                let argsty =
                  match snd constructor_ty with
                  | NT.Ty_arrow _ -> (
                      match
                        snd @@ NT.destruct_arrow_tp (snd constructor_ty)
                      with
                      | NT.Ty_tuple tys -> tys
                      | ty -> [ ty ])
                  | _ -> []
                in
                (* let _ = Printf.printf "zz: %s\n" case.S.constructor.x in *)
                (* let _ = Printf.printf "zz: %s\n" (Type.layout_l argsty) in *)
                (* let _ = *)
                (*   Printf.printf "zz: %s\n" *)
                (*     (Zzdatatype.Datatype.StrList.to_string case.S.args) *)
                (* in *)
                let args =
                  List.map (fun (x, ty) -> { x; ty = (None, ty) })
                  @@ _safe_combine __FILE__ __LINE__ case.S.args argsty
                in
                {
                  constructor =
                    { ty = constructor_ty; x = case.S.constructor.x };
                  args;
                  exp = convert cont case.S.exp ename;
                })
              cases
          in
          mk_t_term ety @@ Match { matched; cases })
        e None

and to_anormal (e : term opttyped) ename : T.term typed =
  T.var_exn_to_exn @@ convert ret_cont e ename

and convert_multi (conts : conts) (es : term opttyped list) : T.term typed =
  (* let open T in *)
  (List.fold_left
     (fun (conts : conts) rhs rest ->
       convert (fun id -> conts (id :: rest)) rhs None)
     conts es)
    []

let id_trans_rev (e : T.id typed) = S.{ ty = Some e.ty; x = e.x }
