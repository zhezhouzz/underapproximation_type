open Ocaml_parser
open Parsetree
open Zzdatatype.Datatype
module T = Languages.Termlang
module L = Languages.Underty

type mode = Under | Tuple
type q = Fa | Ex

let prop_of_ocamlexpr e =
  let prop = Autov.prop_of_ocamlexpr e in
  if Autov.Prop.has_qv prop then
    failwith
      (Sugar.spf
         "Syntax error: refinement proposition cannot contain quantified \
          variables: %s"
      @@ Autov.pretty_layout_prop prop)
  else prop

let mode_of_ocamlexpr e =
  match (Expr.expr_of_ocamlexpr e).x with
  | T.Var "under" -> Under
  | T.Var "tuple" -> Tuple
  | _ -> failwith "mode_of_ocamlexpr"

let core_type_to_qt ct =
  match ct.ptyp_desc with
  | Ptyp_tuple [ { ptyp_desc = Ptyp_var "forall"; _ }; ct ] ->
      (Type.core_type_to_t ct, Fa)
  | Ptyp_tuple [ { ptyp_desc = Ptyp_var "exists"; _ }; ct ] ->
      (Type.core_type_to_t ct, Ex)
  | _ ->
      failwith (Printf.sprintf "prasing prop: wrong label %s" (Type.layout_ ct))

let core_type_of_qt (nt, q) =
  Type.desc_to_ct
    (Ptyp_tuple
       [
         Type.desc_to_ct
           (Ptyp_var (match q with Fa -> "forall" | Ex -> "exists"));
         Type.t_to_core_type nt;
       ])

let undertype_of_ocamlexpr expr =
  let open T in
  let rec aux expr =
    match expr.pexp_desc with
    | Pexp_apply (x, [ prop ]) ->
        let x = Expr.expr_of_ocamlexpr x in
        let normalty, basename =
          match (x.ty, x.x) with
          | Some ty, Var x -> (ty, x)
          | _, _ -> failwith "undertype_of_ocamlexpr"
        in
        let prop = prop_of_ocamlexpr @@ snd prop in
        L.(UnderTy_base { basename; normalty; prop })
    | Pexp_tuple es -> L.UnderTy_tuple (List.map aux es)
    | Pexp_let (_, [ vb ], body) ->
        let id = Pat.pattern_to_slang vb.pvb_pat in
        let argname =
          match id.x with
          | Var name -> name
          | _ -> failwith "undertype_of_ocamlexpr"
        in
        let argty = aux vb.pvb_expr in
        L.(UnderTy_arrow { argname; argty; retty = aux body })
    | _ -> failwith "wrong refinement type"
  in
  aux expr

let undertype_to_ocamlexpr x =
  let open L in
  let rec aux x =
    match x with
    | UnderTy_base { basename; normalty; prop } ->
        let mode = Expr.expr_to_ocamlexpr { ty = None; x = Var "under" } in
        let x =
          Expr.expr_to_ocamlexpr { ty = Some normalty; x = Var basename }
        in
        let prop = Autov.prop_to_ocamlexpr prop in
        Expr.desc_to_ocamlexpr
        @@ Pexp_apply
             (mode, List.map (fun x -> (Asttypes.Nolabel, x)) [ x; prop ])
    | UnderTy_arrow { argname; argty; retty } ->
        Expr.desc_to_ocamlexpr
        @@ Pexp_let
             ( Asttypes.Nonrecursive,
               [
                 {
                   pvb_pat =
                     Pat.dest_to_pat @@ Ppat_var (Location.mknoloc argname);
                   pvb_expr = aux argty;
                   pvb_attributes = [];
                   pvb_loc = Location.none;
                 };
               ],
               aux retty )
    | UnderTy_tuple ts -> Expr.desc_to_ocamlexpr @@ Pexp_tuple (List.map aux ts)
  in
  aux x

let quantified_undertype_of_ocamlexpr e =
  let open L in
  let rec aux (uqvs, eqvs) expr =
    match expr.pexp_desc with
    | Pexp_fun (_, _, arg, expr) ->
        let q, id =
          match arg.ppat_desc with
          | Ppat_constraint (arg, core_type) ->
              let nt, q = core_type_to_qt core_type in
              let arg =
                match arg.ppat_desc with
                | Ppat_var arg -> arg.txt
                | _ -> failwith "parsing: prop function"
              in
              (q, { ty = nt; x = arg })
          | _ -> failwith "parsing: prop function"
        in
        let uqvs, eqvs =
          match q with
          | Fa ->
              if List.length eqvs > 0 then
                failwith "Undertype Syntax Error: not a forall exists form"
              else (uqvs @ [ id ], eqvs)
          | Ex -> (uqvs, eqvs @ [ id ])
        in
        aux (uqvs, eqvs) expr
    | _ -> { uqvs; eqvs; k = undertype_of_ocamlexpr expr }
  in
  aux ([], []) e

let quantified_undertype_to_ocamlexpr L.{ uqvs; eqvs; k = t } =
  let open L in
  let mk_lam (x, qt, e) =
    Expr.desc_to_ocamlexpr
      (Pexp_fun
         ( Asttypes.Nolabel,
           None,
           Pat.dest_to_pat
             (Ppat_constraint
                ( Pat.dest_to_pat (Ppat_var (Location.mknoloc x)),
                  core_type_of_qt qt )),
           e ))
  in
  List.fold_right
    (fun { x; ty } e -> mk_lam (x, (ty, Fa), e))
    uqvs
    (List.fold_right
       (fun { x; ty } e -> mk_lam (x, (ty, Ex), e))
       eqvs (undertype_to_ocamlexpr t))

let layout x = Pprintast.string_of_expression @@ undertype_to_ocamlexpr x

let pretty_layout x =
  let open L in
  let rec aux x =
    match x with
    | UnderTy_base { basename; normalty; prop } ->
        Sugar.spf "[%s:%s | %s]" basename (Type.layout normalty)
          (Autov.pretty_layout_prop prop)
    | UnderTy_arrow { argname; argty; retty } ->
        Sugar.spf "(%s:%s) → %s" argname (aux argty) (aux retty)
    | UnderTy_tuple ts ->
        Sugar.spf "(%s)" @@ Zzdatatype.Datatype.List.split_by_comma aux ts
  in
  aux x

let layout_q x =
  Pprintast.string_of_expression @@ quantified_undertype_to_ocamlexpr x

let layout_qt f L.{ uqvs; eqvs; k = t } =
  let open L in
  let mk_q (q, x, _, e) =
    let q = match q with Fa -> "∀" | Ex -> "∃" in
    Printf.sprintf "%s%s, %s" q x e
  in
  List.fold_right
    (fun { x; ty } e -> mk_q (Fa, x, ty, e))
    uqvs
    (List.fold_right (fun { x; ty } e -> mk_q (Ex, x, ty, e)) eqvs (f t))

let pretty_layout_q x = layout_qt pretty_layout x
