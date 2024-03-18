open Ocaml5_parser
open Parsetree
open Mtyped
open Mutils
open Zzdatatype.Datatype
module Nt = Normalty.Frontend
open Prop
open To_raw_term
open To_lit
open To_notation
open Sugar

let notated (name, t) =
  desc_to_ct
  @@ Ptyp_extension (Location.mknoloc name, PTyp (Nt.t_to_core_type t))

let quantifier_to_pattern (q, u) =
  dest_to_pat
    (Ppat_constraint
       ( dest_to_pat (Ppat_var (Location.mknoloc u.x)),
         notated (Normalty.Connective.qt_to_string q, u.ty) ))

let smt_layout_ty = function
  | Some Nt.T.Ty_bool -> "Bool"
  | Some Nt.T.Ty_int -> "Int"
  | Some (Nt.T.Ty_constructor _) -> "Int"
  | _ -> _failatwith __FILE__ __LINE__ "unimp"

let rec layout_to_smtlib2 = function
  | Lit lit -> layout_typed_lit_to_smtlib2 lit
  | Implies (p1, p2) ->
      spf "(=> %s %s)" (layout_to_smtlib2 p1) (layout_to_smtlib2 p2)
  | And [ p ] -> layout_to_smtlib2 p
  | Or [ p ] -> layout_to_smtlib2 p
  | And ps -> spf "(and %s)" @@ List.split_by " " layout_to_smtlib2 ps
  | Or ps -> spf "(or %s)" @@ List.split_by " " layout_to_smtlib2 ps
  | Not p -> spf "(not %s)" (layout_to_smtlib2 p)
  | Iff (p1, p2) ->
      spf "(= %s %s)" (layout_to_smtlib2 p1) (layout_to_smtlib2 p2)
  | Ite _ -> _failatwith __FILE__ __LINE__ "unimp"
  | Forall { qv; body } ->
      spf "(forall ((%s %s)) %s)" qv.x (smt_layout_ty qv.ty)
        (layout_to_smtlib2 body)
  | Exists { qv; body } ->
      spf "(exists ((%s %s)) %s)" qv.x (smt_layout_ty qv.ty)
        (layout_to_smtlib2 body)

type 't layout_setting = {
  sym_true : string;
  sym_false : string;
  sym_and : string;
  sym_or : string;
  sym_not : string;
  sym_implies : string;
  sym_iff : string;
  sym_forall : string;
  sym_exists : string;
  layout_typedid : ('t, string) typed -> string;
  layout_mp : string -> string;
}

let detailssetting =
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
    layout_typedid = Nt.(fun x -> spf "(%s:%s)" x.x (layout x.ty));
    layout_mp = (fun x -> x);
  }

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
    (* (fun x ->          Printf.spf "(%s:%s)" x.x (Ty.layout x.ty)); *)
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
    sym_forall = "forall ";
    sym_exists = "exists ";
    layout_typedid = (fun x -> x.x);
    layout_mp = (function "==" -> "=" | x -> x);
  }

let layout_prop_
    {
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
  let rec layout = function
    | Lit lit -> (layout_typed_lit lit, true)
    | Implies (p1, p2) ->
        (spf "%s %s %s" (p_layout p1) sym_implies (p_layout p2), false)
    | And [ p ] -> layout p
    | Or [ p ] -> layout p
    | And [ p1; p2 ] -> (spf "%s%s%s" (p_layout p1) sym_and (p_layout p2), false)
    | Or [ p1; p2 ] -> (spf "%s%s%s" (p_layout p1) sym_or (p_layout p2), false)
    | And ps -> (spf "%s" @@ List.split_by sym_and p_layout ps, false)
    | Or ps -> (spf "%s" @@ List.split_by sym_or p_layout ps, false)
    | Not p -> (spf "%s%s" sym_not (p_layout p), true)
    | Iff (p1, p2) -> (spf "%s %s %s" (p_layout p1) sym_iff (p_layout p2), false)
    | Ite (p1, p2, p3) ->
        ( spf "if %s then %s else %s"
            (fst @@ layout p1)
            (fst @@ layout p2)
            (fst @@ layout p3),
          false )
    | Forall { qv; body } ->
        (spf "%s%s, %s" sym_forall (layout_typedid qv) (p_layout body), false)
    | Exists { qv; body } ->
        (spf "%s%s, %s" sym_exists (layout_typedid qv) (p_layout body), false)
  and p_layout p =
    match layout p with str, true -> str | str, false -> spf "(%s)" str
  in
  fun a -> fst @@ layout a

let rec prop_to_expr expr =
  let rec aux e =
    match e with
    | Lit lit -> typed_lit_to_expr lit
    | Implies (e1, e2) ->
        mk_op_apply ("implies", List.map prop_to_expr [ e1; e2 ])
    | Ite (e1, e2, e3) ->
        let e1, e2, e3 = map3 prop_to_expr (e1, e2, e3) in
        mk_ite (e1, e2, e3)
    | Not e -> mk_op_apply ("not", List.map prop_to_expr [ e ])
    | And [] -> failwith "un-imp"
    | And [ x ] -> aux x
    | And (h :: t) -> mk_op_apply ("&&", List.map prop_to_expr [ h; And t ])
    | Or [] -> failwith "un-imp"
    | Or [ x ] -> aux x
    | Or (h :: t) -> mk_op_apply ("||", List.map prop_to_expr [ h; And t ])
    | Iff (e1, e2) -> mk_op_apply ("iff", List.map prop_to_expr [ e1; e2 ])
    | Forall { qv; body } ->
        let qv =
          match qv.ty with
          | Some ty -> qv.x #: ty
          | None -> _failatwith __FILE__ __LINE__ "die"
        in
        mklam
          (quantifier_to_pattern (Normalty.Connective.Fa, qv))
          (prop_to_expr body)
    | Exists { qv; body } ->
        let qv =
          match qv.ty with
          | Some ty -> qv.x #: ty
          | None -> _failatwith __FILE__ __LINE__ "die"
        in
        mklam
          (quantifier_to_pattern (Normalty.Connective.Ex, qv))
          (prop_to_expr body)
  in
  aux expr

let quantifier_of_expr arg =
  match arg.ppat_desc with
  | Ppat_constraint (arg, ct) ->
      let q =
        match get_pat_denoteopt arg with
        | None ->
            Normalty.Connective.Fa
            (* here we assume it has forall by default. *)
            (* _failatwith __FILE__ __LINE__ *)
            (* "quantifier needs be [@forall] or [@exists]" *)
        | Some q -> Normalty.Connective.qt_of_string q
      in
      let arg =
        match arg.ppat_desc with
        | Ppat_var arg -> arg.txt
        | _ -> failwith "parsing: prop function"
      in
      let ty = Nt.core_type_to_t ct in
      (q, arg #: (Some ty))
  | _ -> _failatwith __FILE__ __LINE__ "quantifier needs type notation"

let prop_of_expr expr =
  let rec aux expr =
    match expr.pexp_desc with
    | Pexp_constraint _ -> failwith "parsing: prop does not have type"
    | Pexp_let _ -> failwith "parsing: prop does not have let"
    | Pexp_match _ -> failwith "parsing: prop does not have match"
    | Pexp_apply (func, args) -> (
        (*     let () = *)
        (*       Printf.printf "expr: %s\n" (Pprintast.string_of_expression expr) *)
        (*     in *)
        let f = id_of_expr func in
        let args = List.map snd args in
        (* let () = Printf.printf "f: %s\n" f.x in *)
        match (f.x, args) with
        | "not", [ e1 ] -> Not (aux e1)
        | "not", _ -> failwith "parsing: prop wrong not"
        | "ite", [ e1; e2; e3 ] -> Ite (aux e1, aux e2, aux e3)
        | "ite", _ -> failwith "parsing: prop wrong ite"
        | "implies", [ e1; e2 ] -> Implies (aux e1, aux e2)
        | "implies", _ -> failwith "parsing: prop wrong implies"
        | "#==>", [ e1; e2 ] -> Implies (aux e1, aux e2)
        | "#==>", _ -> failwith "parsing: prop wrong implies"
        | "iff", [ e1; e2 ] -> Iff (aux e1, aux e2)
        | "iff", _ -> failwith "parsing: prop wrong iff"
        | "&&", [ a; b ] -> And [ aux a; aux b ]
        | "&&", _ -> failwith "parsing: prop wrong and"
        | "||", [ a; b ] -> Or [ aux a; aux b ]
        | "||", _ -> failwith "parsing: prop wrong or"
        | "=", _ -> failwith "please use == instead of ="
        | _, _ -> Lit (typed_lit_of_expr expr))
    | Pexp_ifthenelse (e1, e2, Some e3) ->
        let p1, p2, p3 = map3 aux (e1, e2, e3) in
        And [ Implies (p1, p2); Implies (Not p1, p3) ]
        (* Ite (aux e1, aux e2, aux e3) *)
    | Pexp_ifthenelse (_, _, None) -> raise @@ failwith "no else branch in ite"
    | Pexp_fun (_, _, arg, expr) -> (
        let q, qv = quantifier_of_expr arg in
        let body = aux expr in
        match q with
        | Normalty.Connective.Fa -> Forall { qv; body }
        | Normalty.Connective.Ex -> Exists { qv; body })
    | Pexp_construct _ ->
        (* let () = *)
        (*   Printf.printf "expr: %s\n" (Pprintast.string_of_expression expr) *)
        (* in *)
        Lit (typed_lit_of_expr expr)
    | Pexp_tuple _ | Pexp_ident _ | Pexp_constant _ ->
        Lit (typed_lit_of_expr expr)
    | _ ->
        raise
        @@ failwith
             (spf "not imp client parsing:%s"
             @@ Pprintast.string_of_expression expr)
  in
  aux expr

let layout_prop__raw x = Pprintast.string_of_expression @@ prop_to_expr x
let layout_prop = layout_prop_ psetting
let layout_prop_to_coq = layout_prop_ coqsetting
