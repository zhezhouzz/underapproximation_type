open Ocaml_parser
open Parsetree
module L = Prop.T

type label = Fa | Ex

let layout_ct t =
  let _ = Format.flush_str_formatter () in
  Pprintast.core_type Format.str_formatter t;
  Format.flush_str_formatter ()

let core_type_to_label ct =
  match ct.ptyp_desc with
  (* | Ptyp_constr (name, []) -> ( *)
  (*     match Longident.last name.txt with *)
  (*     | "forall" -> Fa *)
  (*     | "exists" -> Ex *)
  (*     | _ -> *)
  (*         failwith *)
  (*           (Printf.sprintf "prasing prop: wrong label %s" (layout_ct ct))) *)
  | Ptyp_var "fa" -> Fa
  | Ptyp_var "ex" -> Ex
  | _ -> failwith (Printf.sprintf "prasing prop: wrong label %s" (layout_ct ct))

let label_to_core_type x =
  let ct = match x with Fa -> Ptyp_var "fa" | Ex -> Ptyp_var "ex" in
  {
    ptyp_desc = ct;
    ptyp_loc = Location.none;
    ptyp_loc_stack = [];
    ptyp_attributes = [];
  }

let prop_of_ocamlexpr expr =
  (* TODO: parse type of prop *)
  let handle_id id =
    match Longident.flatten id.Location.txt with
    | [ x ] -> L.{ ty = Smtty.T.Int; x }
    | ids ->
        failwith
          (Printf.sprintf "expr, handel id: %s"
          @@ Zzdatatype.Datatype.StrList.to_string ids)
  in
  (* let id_to_var id = L.(Var (handle_id id)) in *)
  let expr_to_id e =
    match e.pexp_desc with
    | Pexp_ident id -> handle_id id
    | _ -> failwith "parsing: prop does not have nested application"
  in
  let rec aux expr =
    match expr.pexp_desc with
    | Pexp_tuple _ -> failwith "parsing: prop does not have tuple"
    | Pexp_constraint _ -> failwith "parsing: prop does not have type"
    | Pexp_ident id -> L.(Var (handle_id id))
    | Pexp_construct (id, None) -> (
        match Longident.last id.txt with
        | "true" -> L.True
        | "false" -> L.Not L.True
        | _ -> raise @@ failwith "do not support complicate literal")
    | Pexp_construct (_, Some _) -> raise @@ failwith "Pexp_construct"
    (* | Pexp_constant (Pconst_string ("true", _, None)) -> L.True *)
    (* | Pexp_constant (Pconst_string ("false", _, None)) -> L.Not L.True *)
    | Pexp_constant _ -> raise @@ failwith "do not support complicate literal"
    | Pexp_let _ -> failwith "parsing: prop does not have let"
    | Pexp_apply (func, args) -> (
        let f = (expr_to_id func).L.x in
        let args = List.map snd args in
        match (f, args) with
        | "not", [ e1 ] -> L.Not (aux e1)
        | "not", _ -> failwith "parsing: prop wrong not"
        | "implies", [ e1; e2 ] -> L.Implies (aux e1, aux e2)
        | "implies", _ -> failwith "parsing: prop wrong implies"
        | "iff", [ e1; e2 ] -> L.Iff (aux e1, aux e2)
        | "iff", _ -> failwith "parsing: prop wrong iff"
        | "&&", [ a; b ] -> L.And [ aux a; aux b ]
        | "&&", _ -> failwith "parsing: prop wrong and"
        | "||", [ a; b ] -> L.Or [ aux a; aux b ]
        | "||", _ -> failwith "parsing: prop wrong or"
        | f, args ->
            let args = List.map (fun x -> expr_to_id x) args in
            L.MethodPred (f, args))
    | Pexp_ifthenelse (e1, e2, Some e3) -> L.(Ite (aux e1, aux e2, aux e3))
    | Pexp_ifthenelse (_, _, None) -> raise @@ failwith "no else branch in ite"
    | Pexp_match _ -> failwith "parsing: prop does not have match"
    | Pexp_fun (_, _, arg, expr) -> (
        let label, (uty, u) =
          match arg.ppat_desc with
          | Ppat_constraint (arg, core_type) ->
              let label = core_type_to_label core_type in
              let arg =
                match arg.ppat_desc with
                | Ppat_var arg -> arg.txt
                | _ -> failwith "parsing: prop function"
              in
              (label, (Smtty.T.Int, arg))
          | _ -> failwith "parsing: prop function"
        in
        let body = aux expr in
        match label with
        | Fa -> L.Forall ({ ty = uty; x = u }, body)
        | Ex -> L.Exists ({ ty = uty; x = u }, body))
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

let prop_to_expr prop =
  let expr_to_arg e = (Asttypes.Nolabel, e) in
  let rec aux e =
    let aux' x = expr_to_arg @@ aux x in
    match e with
    | P.True ->
        desc_to_ocamlexpr
          (Pexp_construct (Location.mknoloc @@ Longident.Lident "true", None))
    | P.Not P.True ->
        desc_to_ocamlexpr
          (Pexp_construct (Location.mknoloc @@ Longident.Lident "false", None))
    | P.Var b -> string_to_expr b.x
    | P.MethodPred (mp, args) ->
        desc_to_ocamlexpr
          (Pexp_apply
             ( string_to_expr mp,
               List.map
                 (fun name -> expr_to_arg @@ string_to_expr name.P.x)
                 args ))
    | P.Implies (e1, e2) ->
        desc_to_ocamlexpr
          (Pexp_apply
             ( string_to_expr "implies",
               List.map (fun x -> expr_to_arg @@ aux x) [ e1; e2 ] ))
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
                      label_to_core_type Fa )),
               aux body ))
    | P.Exists (u, body) ->
        desc_to_ocamlexpr
          (Pexp_fun
             ( Asttypes.Nolabel,
               None,
               dest_to_pat
                 (Ppat_constraint
                    ( dest_to_pat (Ppat_var (Location.mknoloc u.x)),
                      label_to_core_type Ex )),
               aux body ))
  in

  aux prop

let layout prop = Pprintast.string_of_expression @@ prop_to_expr prop

open P

let sym_and = " ∧ "
let sym_or = " ∨ "
let sym_not = "¬"
let sym_implies = "=>"
let sym_iff = "<=>"

let is_op op =
  match op with "==" | "<" | ">" | "<=" | ">=" -> true | _ -> false

open Printf
open Zzdatatype.Datatype

let pretty_layout x =
  let rec layout = function
    | True -> "⊤"
    | Not True -> "⊥"
    | Var b -> (
        match b.ty with Smtty.T.Bool -> sprintf "(%s:𝓑 )" b.x | _ -> b.x)
    | MethodPred (mp, args) ->
        if is_op mp then
          match args with
          | [ a; b ] -> sprintf "(%s %s %s)" a.x mp b.x
          | _ -> sprintf "%s(%s)" mp (List.split_by_comma (fun x -> x.x) args)
        else
          sprintf "(%s %s)" mp
            (* (Method_predicate.poly_name mp) *)
            (List.split_by " " (fun x -> x.x) args)
    | Implies (p1, p2) ->
        sprintf "(%s %s %s)" (layout p1) sym_implies (layout p2)
    | And ps -> sprintf "(%s)" @@ List.split_by sym_and layout ps
    | Or ps -> sprintf "(%s)" @@ List.split_by sym_or layout ps
    | Not p -> sprintf "(%s %s)" sym_not @@ layout p
    | Iff (p1, p2) -> sprintf "(%s %s %s)" (layout p1) sym_iff (layout p2)
    | Ite (p1, p2, p3) ->
        sprintf "(if %s then %s else %s)" (layout p1) (layout p2) (layout p3)
    | Forall (u, body) -> sprintf "(∀ %s, %s)" u.x (layout body)
    | Exists (u, body) -> sprintf "(∃ %s, %s)" u.x (layout body)
  in
  layout x
