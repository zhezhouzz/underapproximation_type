open Ocaml_parser
open Parsetree
module L = Ast
module Ty = Normalty.Ast.T
module Q = Normalty.Ast.Q
open Normalty.Ast.Ntyped
open Normalty.Frontend

(* type label = Fa of Ty.t | Ex of Ty.t *)

let layout_ct t =
  let _ = Format.flush_str_formatter () in
  Pprintast.core_type Format.str_formatter t;
  Format.flush_str_formatter ()

let ptyp_desc_to_ct ct =
  {
    ptyp_desc = ct;
    ptyp_loc = Location.none;
    ptyp_loc_stack = [];
    ptyp_attributes = [];
  }

(* let core_type_to_label ct = *)
(*   match ct.ptyp_desc with *)
(*   | Ptyp_tuple [ { ptyp_desc = Ptyp_var "fa"; _ }; t ] -> Fa (core_type_to_ty t) *)
(*   | Ptyp_tuple [ { ptyp_desc = Ptyp_var "ex"; _ }; t ] -> Ex (core_type_to_ty t) *)
(*   | _ -> failwith (Printf.sprintf "prasing prop: wrong label %s" (layout_ct ct)) *)

(* let label_to_core_type x = *)
(*   let ct = *)
(*     match x with *)
(*     | Fa ct -> *)
(*         Ptyp_tuple [ ptyp_desc_to_ct @@ Ptyp_var "fa"; ty_to_core_type ct ] *)
(*     | Ex ct -> *)
(*         Ptyp_tuple [ ptyp_desc_to_ct @@ Ptyp_var "ex"; ty_to_core_type ct ] *)
(*   in *)
(*   ptyp_desc_to_ct ct *)

(* NOTE: should we parse type here? or is the prop is typed? *)
let default_type = Ty.Ty_int

let handle_id id =
  match Longident.flatten id.Location.txt with
  | [ x ] -> { ty = default_type; x }
  | ids ->
      failwith
        (Printf.sprintf "expr, handel id: %s"
        @@ Zzdatatype.Datatype.StrList.to_string ids)

let rec lit_of_ocamlexpr e =
  match e.pexp_desc with
  | Pexp_ident id -> L.AVar (handle_id id)
  | Pexp_constant (Pconst_integer (istr, None)) -> L.ACint (int_of_string istr)
  | Pexp_constant _ -> raise @@ failwith "do not support complicate literal"
  | Pexp_apply (func, [ a; b ]) ->
      let a = lit_of_ocamlexpr @@ snd a in
      let b = lit_of_ocamlexpr @@ snd b in
      let f =
        match func.pexp_desc with
        | Pexp_ident id -> (handle_id id).x
        | _ -> failwith "wrong method predicate"
      in
      if L.is_op f then L.AOp2 (f, a, b)
      else
        failwith
        @@ Printf.sprintf "parsing: not a op (%s)"
        @@ Pprintast.string_of_expression e
  | _ ->
      failwith
      @@ Printf.sprintf "parsing: not a op (%s)"
      @@ Pprintast.string_of_expression e

let prop_of_ocamlexpr expr =
  let rec aux expr =
    match expr.pexp_desc with
    | Pexp_tuple _ -> failwith "parsing: prop does not have tuple"
    | Pexp_constraint _ -> failwith "parsing: prop does not have type"
    | Pexp_ident _ | Pexp_constant _ -> L.Lit (lit_of_ocamlexpr expr)
    | Pexp_construct (id, None) -> (
        match Longident.last id.txt with
        | "true" -> L.mk_true
        | "false" -> L.mk_false
        | _ -> raise @@ failwith "do not support complicate literal")
    | Pexp_construct (_, Some _) -> raise @@ failwith "Pexp_construct"
    | Pexp_let _ -> failwith "parsing: prop does not have let"
    | Pexp_apply (func, args) -> (
        let f =
          match func.pexp_desc with
          | Pexp_ident id -> (handle_id id).x
          | _ -> failwith "wrong method predicate"
        in
        let args = List.map snd args in
        match (f, args) with
        | "not", [ e1 ] -> L.Not (aux e1)
        | "not", _ -> failwith "parsing: prop wrong not"
        | "ite", [ e1; e2; e3 ] -> L.Ite (aux e1, aux e2, aux e3)
        | "ite", _ -> failwith "parsing: prop wrong ite"
        | "implies", [ e1; e2 ] -> L.Implies (aux e1, aux e2)
        | "implies", _ -> failwith "parsing: prop wrong implies"
        | "iff", [ e1; e2 ] -> L.Iff (aux e1, aux e2)
        | "iff", _ -> failwith "parsing: prop wrong iff"
        | "&&", [ a; b ] -> L.And [ aux a; aux b ]
        | "&&", _ -> failwith "parsing: prop wrong and"
        | "||", [ a; b ] -> L.Or [ aux a; aux b ]
        | "||", _ -> failwith "parsing: prop wrong or"
        | "=", _ -> failwith "please use == instead of = "
        | f, args ->
            let args = List.map lit_of_ocamlexpr args in
            L.MethodPred (f, args))
    | Pexp_ifthenelse (e1, e2, Some e3) -> L.(Ite (aux e1, aux e2, aux e3))
    | Pexp_ifthenelse (_, _, None) -> raise @@ failwith "no else branch in ite"
    | Pexp_match _ -> failwith "parsing: prop does not have match"
    | Pexp_fun (_, _, arg, expr) -> (
        let label, u =
          match arg.ppat_desc with
          | Ppat_constraint (arg, core_type) ->
              let label =
                match core_type_to_notated_t core_type with
                | Some name, ty -> (Q.of_string name, ty)
                | _ -> failwith "parsing: prop function"
              in
              let arg =
                match arg.ppat_desc with
                | Ppat_var arg -> arg.txt
                | _ -> failwith "parsing: prop function"
              in
              (label, arg)
          | _ -> failwith "parsing: prop function"
        in
        let body = aux expr in
        match label with
        | Q.Fa, uty -> L.Forall ({ ty = uty; x = u }, body)
        | Q.Ex, uty -> L.Exists ({ ty = uty; x = u }, body))
    | _ ->
        raise
        @@ failwith
             (Printf.sprintf "not imp client parsing:%s"
             @@ Pprintast.string_of_expression expr)
  in
  aux expr

module P = L

let desc_to_ocamlexpr desc =
  {
    pexp_desc = desc;
    pexp_loc = Location.none;
    pexp_loc_stack = [];
    pexp_attributes = [];
  }

let string_to_expr name =
  desc_to_ocamlexpr (Pexp_ident (Location.mknoloc @@ Longident.Lident name))

let dest_to_pat pat =
  {
    ppat_desc = pat;
    ppat_loc = Location.none;
    ppat_loc_stack = [];
    ppat_attributes = [];
  }

let rec lit_to_expr lit =
  let open L in
  match lit with
  | ACint n ->
      desc_to_ocamlexpr
      @@ Pexp_constant (Pconst_integer (string_of_int n, None))
  | ACbool true ->
      desc_to_ocamlexpr
        (Pexp_construct (Location.mknoloc @@ Longident.Lident "true", None))
  | ACbool false ->
      desc_to_ocamlexpr
        (Pexp_construct (Location.mknoloc @@ Longident.Lident "false", None))
  | AVar id -> string_to_expr id.x
  | AOp2 (mp, a, b) ->
      let a = lit_to_expr a in
      let b = lit_to_expr b in
      desc_to_ocamlexpr
        (Pexp_apply
           (string_to_expr mp, [ (Asttypes.Nolabel, a); (Asttypes.Nolabel, b) ]))

let prop_to_expr prop =
  let rec aux e =
    let aux' x = (Asttypes.Nolabel, aux x) in
    match e with
    | P.Lit lit -> lit_to_expr lit
    | P.MethodPred (mp, args) ->
        desc_to_ocamlexpr
          (Pexp_apply
             ( string_to_expr mp,
               List.map (fun x -> (Asttypes.Nolabel, lit_to_expr x)) args ))
    | P.Implies (e1, e2) ->
        desc_to_ocamlexpr
          (Pexp_apply (string_to_expr "implies", List.map aux' [ e1; e2 ]))
    | P.Ite (e1, e2, e3) ->
        desc_to_ocamlexpr (Pexp_ifthenelse (aux e1, aux e2, Some (aux e3)))
    | P.Not e ->
        desc_to_ocamlexpr
          (Pexp_apply (string_to_expr "not", List.map aux' [ e ]))
    | P.And [] -> failwith "un-imp"
    | P.And [ x ] -> aux x
    | P.And (h :: t) ->
        desc_to_ocamlexpr
          (Pexp_apply (string_to_expr "&&", List.map aux' [ h; P.And t ]))
    | P.Or [] -> failwith "un-imp"
    | P.Or [ x ] -> aux x
    | P.Or (h :: t) ->
        desc_to_ocamlexpr
          (Pexp_apply (string_to_expr "||", List.map aux' [ h; P.Or t ]))
    | P.Iff (e1, e2) ->
        desc_to_ocamlexpr
          (Pexp_apply (string_to_expr "iff", List.map aux' [ e1; e2 ]))
    | P.Forall (u, body) ->
        desc_to_ocamlexpr
          (Pexp_fun
             ( Asttypes.Nolabel,
               None,
               dest_to_pat
                 (Ppat_constraint
                    ( dest_to_pat (Ppat_var (Location.mknoloc u.x)),
                      notated_t_to_core_type (Some (Q.to_string Fa), u.ty) )),
               aux body ))
    | P.Exists (u, body) ->
        desc_to_ocamlexpr
          (Pexp_fun
             ( Asttypes.Nolabel,
               None,
               dest_to_pat
                 (Ppat_constraint
                    ( dest_to_pat (Ppat_var (Location.mknoloc u.x)),
                      notated_t_to_core_type (Some (Q.to_string Q.Ex), u.ty) )),
               aux body ))
  in

  aux prop

let layout prop = Pprintast.string_of_expression @@ prop_to_expr prop

type layout_setting = {
  sym_true : string;
  sym_false : string;
  sym_and : string;
  sym_or : string;
  sym_not : string;
  sym_implies : string;
  sym_iff : string;
  sym_forall : string;
  sym_exists : string;
  layout_typedid : string typed -> string;
  layout_mp : string -> string;
}

open Printf
open Zzdatatype.Datatype

let psetting =
  {
    sym_true = "⊤";
    sym_false = "⊥";
    sym_and = " ∧ ";
    sym_or = " ∨ ";
    sym_not = "¬";
    sym_implies = "=>";
    sym_iff = "<=>";
    sym_forall = "∀";
    sym_exists = "∃";
    layout_typedid = (fun x -> x.x);
    (* (fun x ->          Printf.sprintf "(%s:%s)" x.x (Ty.layout x.ty)); *)
    layout_mp = (fun x -> x);
  }

let coqsetting =
  {
    sym_true = "True";
    sym_false = "False";
    sym_and = "/\\ ";
    sym_or = " \\/ ";
    sym_not = "~";
    sym_implies = "->";
    sym_iff = "<->";
    sym_forall = "forall";
    sym_exists = "exists";
    layout_typedid =
      (fun x -> sprintf "(%s:%s)" x.x (Normalty.Frontend.layout x.ty));
    layout_mp = (function "==" -> "=" | x -> x);
  }

open P
open Sugar

let lit_pretty_layout_ { sym_true; sym_false; layout_typedid; layout_mp; _ } =
  let rec aux = function
    | ACint n -> string_of_int n
    | ACbool false -> sym_false
    | ACbool true -> sym_true
    | AVar id -> layout_typedid id
    | AOp2 (mp, a, b) ->
        if is_op mp then sprintf "(%s %s %s)" (aux a) (layout_mp mp) (aux b)
        else _failatwith __FILE__ __LINE__ @@ spf "unknown op %s" mp
  in
  aux

let _pretty_layout s =
  let {
    sym_and;
    sym_or;
    sym_not;
    sym_implies;
    sym_iff;
    sym_forall;
    sym_exists;
    layout_typedid;
    _;
  } =
    s
  in
  let pretty_layout x =
    let rec layout = function
      | Lit lit -> lit_pretty_layout_ s lit
      | MethodPred (mp, args) ->
          let args = List.map (lit_pretty_layout_ s) args in
          if is_bop mp then
            match args with
            | [ a; b ] -> sprintf "(%s %s %s)" a mp b
            | _ -> _failatwith __FILE__ __LINE__ ""
          else sprintf "(%s %s)" mp (List.split_by " " (fun x -> x) args)
      | Implies (p1, p2) ->
          sprintf "(%s %s %s)" (layout p1) sym_implies (layout p2)
      | And ps -> sprintf "(%s)" @@ List.split_by sym_and layout ps
      | Or ps -> sprintf "(%s)" @@ List.split_by sym_or layout ps
      | Not p -> sprintf "(%s %s)" sym_not @@ layout p
      | Iff (p1, p2) -> sprintf "(%s %s %s)" (layout p1) sym_iff (layout p2)
      | Ite (p1, p2, p3) ->
          sprintf "(if %s then %s else %s)" (layout p1) (layout p2) (layout p3)
      | Forall (u, body) ->
          sprintf "(%s %s, %s)" sym_forall (layout_typedid u) (layout body)
      | Exists (u, body) ->
          sprintf "(%s %s, %s)" sym_exists (layout_typedid u) (layout body)
    in
    layout x
  in
  pretty_layout

let pretty_layout = _pretty_layout psetting
let pretty_layout_lit = lit_pretty_layout_ psetting
let coq_layout = _pretty_layout coqsetting
