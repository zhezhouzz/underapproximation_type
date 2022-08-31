open Ocaml_parser
open Parsetree
open Zzdatatype.Datatype
module T = Languages.Termlang
module L = Languages.Underty
module Ntyped = Languages.Ntyped

type mode = Under | Tuple

let prop_of_ocamlexpr e =
  let prop = Autov.prop_of_ocamlexpr e in
  let uqvs, eqvs, _ = Autov.Prop.to_fe_nf prop in
  let () =
    if List.length uqvs != 0 then
      failwith
        (Sugar.spf
           "Syntax error: refinement proposition cannot contain universial \
            quantified variables: %s"
        @@ Autov.pretty_layout_prop prop)
    else ()
  in
  let () =
    if List.length eqvs != 0 then
      failwith
        (Sugar.spf
           "Syntax error: refinement proposition cannot contain existential \
            quantified variables: %s"
        @@ Autov.pretty_layout_prop prop)
    else ()
  in
  prop

let mode_of_ocamlexpr e =
  match (Expr.expr_of_ocamlexpr e).x with
  | T.Var "under" -> Under
  | T.Var "tuple" -> Tuple
  | _ -> failwith "mode_of_ocamlexpr"

let undertype_of_ocamlexpr expr =
  let open T in
  let rec parse_hidden expr =
    match expr.pexp_desc with
    | Pexp_fun (_, _, var, e) ->
        let hidden_vars =
          List.map (fun x ->
              match x.ty with
              | None -> failwith "undertype_of_ocamlexpr"
              | Some (_, ty) -> Ntyped.{ x = x.x; ty })
          @@ Pat.patten_to_typed_ids var
        in
        (hidden_vars, aux e)
    | _ -> ([], aux expr)
  and aux expr =
    match expr.pexp_desc with
    | Pexp_apply (x, [ prop ]) ->
        let x = Expr.expr_of_ocamlexpr x in
        let normalty, basename =
          match (x.ty, x.x) with
          | Some (_, ty), Var x -> (ty, x)
          | _, _ -> failwith "undertype_of_ocamlexpr"
        in
        let prop = prop_of_ocamlexpr @@ snd prop in
        L.(UnderTy_base { basename; normalty; prop })
    | Pexp_tuple es -> L.UnderTy_tuple (List.map aux es)
    | Pexp_let (_, [ vb ], body) ->
        let id = Pat.patten_to_typed_ids vb.pvb_pat in
        let argname =
          match id with
          | [ id ] -> id.x
          | _ -> failwith "undertype_of_ocamlexpr"
        in
        let hidden_vars, argty = parse_hidden vb.pvb_expr in
        L.(UnderTy_arrow { argname; hidden_vars; argty; retty = aux body })
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
          Expr.expr_to_ocamlexpr
            { ty = Some (None, normalty); x = Var basename }
        in
        let prop = Autov.prop_to_ocamlexpr prop in
        Expr.desc_to_ocamlexpr
        @@ Pexp_apply
             (mode, List.map (fun x -> (Asttypes.Nolabel, x)) [ x; prop ])
    | UnderTy_arrow { argname; hidden_vars; argty; retty } ->
        let patten =
          Pat.typed_ids_to_pattens
          @@ List.map
               (fun x ->
                 Languages.Termlang.
                   { x = x.Ntyped.x; ty = Some (None, x.Ntyped.ty) })
               hidden_vars
        in
        Expr.desc_to_ocamlexpr
        @@ Pexp_let
             ( Asttypes.Nonrecursive,
               [
                 {
                   pvb_pat =
                     Pat.dest_to_pat @@ Ppat_var (Location.mknoloc argname);
                   pvb_expr =
                     Expr.desc_to_ocamlexpr
                     @@ Pexp_fun (Asttypes.Nolabel, None, patten, aux argty);
                   pvb_attributes = [];
                   pvb_loc = Location.none;
                 };
               ],
               aux retty )
    | UnderTy_tuple ts -> Expr.desc_to_ocamlexpr @@ Pexp_tuple (List.map aux ts)
  in
  aux x

let layout x = Pprintast.string_of_expression @@ undertype_to_ocamlexpr x

let pretty_layout x =
  let open L in
  let rec aux x =
    match x with
    | UnderTy_base
        { basename; prop = Autov.Prop.(Lit (AOp2 ("==", AVar id, ACint i))); _ }
      when String.equal basename id.x ->
        Sugar.spf "[%i]" i
    | UnderTy_base
        {
          basename;
          prop = Autov.Prop.(Lit (AOp2 ("==", AVar id, ACbool b)));
          _;
        }
      when String.equal basename id.x ->
        Sugar.spf "[%b]" b
    | UnderTy_base { basename; normalty; prop } ->
        Sugar.spf "[%s:%s | %s]" basename (Type.layout normalty)
          (Autov.pretty_layout_prop prop)
    | UnderTy_arrow { argname; hidden_vars; argty; retty } ->
        let argname_f ty =
          if L.is_fv_in argname retty then Sugar.spf "%s:%s" argname ty else ty
        in
        let hidden_vars_f ty =
          match hidden_vars with
          | [] -> ty
          | _ ->
              Sugar.spf "|%s ▷%s|"
                (List.split_by_comma (fun x -> x.Ntyped.x) hidden_vars)
                ty
        in
        Sugar.spf "%s → %s"
          (argname_f @@ hidden_vars_f @@ aux argty)
          (aux retty)
    | UnderTy_tuple ts ->
        Sugar.spf "(%s)" @@ Zzdatatype.Datatype.List.split_by_comma aux ts
  in
  aux x
