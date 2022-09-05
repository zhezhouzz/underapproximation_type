open Ocaml_parser
open Parsetree
module L = Languages.Qunderty
open Languages.Ntyped
open Quantified
module Type = Normalty.Frontend

let core_type_to_qt ct =
  match ct.ptyp_desc with
  | Ptyp_tuple [ { ptyp_desc = Ptyp_var "forall"; _ }; ct ] ->
      (Type.core_type_to_t ct, Fa)
  (* | Ptyp_tuple [ { ptyp_desc = Ptyp_var "exists"; _ }; ct ] -> *)
  (*     (Type.core_type_to_t ct, Ex) *)
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

let quantified_undertype_of_ocamlexpr e =
  let open L in
  let rec aux uqvs expr =
    match expr.pexp_desc with
    | Pexp_fun (_, _, arg, expr) ->
        let _, id =
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
        let uqvs = uqvs @ [ id ] in
        aux uqvs expr
    | _ -> { qvs = uqvs; qbody = Underty.undertype_of_ocamlexpr expr }
  in
  aux [] e

let quantified_undertype_to_ocamlexpr L.{ qvs; qbody = t } =
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
    qvs
    (Underty.undertype_to_ocamlexpr t)

let layout x =
  Pprintast.string_of_expression @@ quantified_undertype_to_ocamlexpr x

let pretty_layout x =
  let open L in
  layout_qt x.qvs [] (Underty.pretty_layout x.qbody)
