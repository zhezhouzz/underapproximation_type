open Ocaml_parser
open Parsetree
open Zzdatatype.Datatype
module T = Languages.Termlang
module L = Languages.Underty

type mode = Under | Tuple

let mode_of_ocamlexpr e =
  match (Expr.expr_of_ocamlexpr e).x with
  | T.Var "under" -> Under
  | T.Var "tuple" -> Tuple
  | _ -> failwith "mode_of_ocamlexpr"

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
        let prop = Autov.prop_of_ocamlexpr @@ snd prop in
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

let pretty_layout_typectx ctx =
  List.split_by "; "
    (fun (name, ty) -> Printf.sprintf "%s:%s" name (pretty_layout ty))
    ctx

let pretty_layout_judge f ctx (e, r) =
  Printf.sprintf "%s⊢\n%s :\n%s\n"
    (pretty_layout_typectx ctx)
    (Expr.layout @@ T.erase_type @@ f e)
    (pretty_layout r)
