module P = Ast.P
open Ocaml_parser
open Parsetree
open Datatype

let expr_to_string e =
  match e.pexp_desc with
  | Pexp_ident id -> (
      match Longident.flatten id.txt with
      | [ name ] -> name
      | _ -> failwith "un-supported")
  | _ -> failwith "un-supported"

let expr_to_prop expr =
  let rec aux expr =
    match expr.pexp_desc with
    | Pexp_constant (Pconst_string ("true", _, None)) -> P.True
    | Pexp_constant (Pconst_string ("false", _, None)) -> P.Not P.True
    | Pexp_constant _ -> raise @@ failwith "do not support complicate literal"
    | Pexp_ident _ -> P.Bvar (expr_to_string expr)
    | Pexp_construct (id, None) -> (
        match Longident.last id.txt with
        | "true" -> P.True
        | "false" -> P.Not P.True
        | _ -> raise @@ failwith "do not support complicate literal")
    | Pexp_construct (_, Some _) -> raise @@ failwith "Pexp_construct"
    | Pexp_ifthenelse (_, _, None) ->
        raise @@ failwith "Pexp_ifthenelse: no else"
    | Pexp_ifthenelse (cond, e1, Some e2) -> P.Ite (aux cond, aux e1, aux e2)
    | Pexp_apply (func, args) -> (
        let funcname = expr_to_string func in
        let args = List.map snd args in
        match (funcname, args) with
        | "&&", [ a; b ] -> P.And [ aux a; aux b ]
        | "||", [ a; b ] -> P.Or [ aux a; aux b ]
        | "implies", [ a; b ] -> P.Implies (aux a, aux b)
        | "iff", [ a; b ] -> P.Iff (aux a, aux b)
        | "not", [ a ] -> P.Not (aux a)
        | _ -> P.Pred (funcname, List.map expr_to_string args))
    | _ -> raise @@ failwith "parse_propositional_term"
  in
  aux expr

let string_to_expr name =
  Value.dummy_expr (Pexp_ident (Value.string_to_loc name))

let prop_to_expr prop =
  let expr_to_arg e = (Asttypes.Nolabel, e) in
  let rec aux e =
    let aux' x = expr_to_arg @@ aux x in
    match e with
    | P.True -> Value.value_to_expr (Ast.V.B true)
    | P.Bvar b -> string_to_expr b
    | P.Pred (mp, args) ->
        Value.dummy_expr
          (Pexp_apply
             ( string_to_expr mp,
               List.map (fun name -> expr_to_arg @@ string_to_expr name) args ))
    | P.Implies (e1, e2) ->
        Value.dummy_expr
          (Pexp_apply
             ( string_to_expr "implies",
               List.map (fun x -> expr_to_arg @@ aux x) [ e1; e2 ] ))
    | P.Ite (e1, e2, e3) ->
        Value.dummy_expr (Pexp_ifthenelse (aux e1, aux e2, Some (aux e3)))
    | P.Not e ->
        Value.dummy_expr
          (Pexp_apply (string_to_expr "not", List.map aux' [ e ]))
    | P.And [] -> failwith "un-imp"
    | P.And [ x ] -> aux x
    | P.And (h :: t) ->
        Value.dummy_expr
          (Pexp_apply (string_to_expr "&&", List.map aux' [ h; P.And t ]))
    | P.Or [] -> failwith "un-imp"
    | P.Or [ x ] -> aux x
    | P.Or (h :: t) ->
        Value.dummy_expr
          (Pexp_apply (string_to_expr "||", List.map aux' [ h; P.Or t ]))
    | P.Iff (e1, e2) ->
        Value.dummy_expr
          (Pexp_apply (string_to_expr "iff", List.map aux' [ e1; e2 ]))
    | P.PointTo (a, b) -> aux (P.Pred ("pointto", [ a; b ]))
    | P.Forall ([], body) -> aux body
    | P.Forall ((tp, h) :: t, body) ->
        Value.dummy_expr
          (Pexp_fun
             ( Asttypes.Nolabel,
               None,
               Pat.(slang_to_pattern (L.Var (Some tp, [ h ]))),
               aux (P.Forall (t, body)) ))
    | P.Exists ([], body) -> aux body
    | P.Exists (qv, body) ->
        let qv =
          List.map
            (fun (tp, h) ->
              Expr.expr_to_ocamlexpr @@ Pat.L.Var (Some tp, [ h ]))
            qv
        in
        Value.dummy_expr (Pexp_tuple (qv @ [ aux body ]))
  in
  aux prop

let layout prop = Pprintast.string_of_expression @@ prop_to_expr prop
