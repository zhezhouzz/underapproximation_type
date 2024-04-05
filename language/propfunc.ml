open Syntax
open Typedlang
open Zzdatatype.Datatype
open Sugar

type t = Nt.t

let unfold_and prop =
  let rec aux = function
    | [] -> []
    | And l :: l' -> aux (l @ l')
    | prop :: l' -> prop :: aux l'
  in
  let l = aux prop in
  List.slow_rm_dup eq_prop l

let smart_and l =
  let l = unfold_and l in
  if List.exists is_false l then mk_false
  else
    match List.filter (fun p -> not (is_true p)) l with
    | [] -> mk_true
    | [ x ] -> x
    | l -> And l

let unfold_or prop =
  let rec aux = function
    | [] -> []
    | Or l :: l' -> aux (l @ l')
    | prop :: l' -> prop :: aux l'
  in
  let l = aux prop in
  List.slow_rm_dup eq_prop l

let smart_or l =
  let l = unfold_or l in
  if List.exists is_true l then mk_true
  else
    match List.filter (fun p -> not (is_false p)) l with
    | [] -> mk_false
    | [ x ] -> x
    | l -> Or l

let smart_add_to a prop =
  match get_cbool a with
  | Some true -> prop
  | Some false -> mk_false
  | None -> (
      match prop with
      | And props -> smart_and (a :: props)
      | _ -> smart_and [ a; prop ])

let smart_implies a prop =
  match get_cbool a with
  | Some true -> prop
  | Some false -> mk_true
  | None -> Implies (a, prop)

let lit_is_v_eq_lit lit v =
  match lit.x with
  | AAppOp (op, [ a; b ]) when String.equal op.x "==" -> (
      match (a.x, b.x) with
      | AVar a, _ when String.equal a.x v -> Some b
      | _, AVar b when String.equal b.x v -> Some a
      | _ -> None)
  | _ -> None

let prop_is_v_eq_lit prop v =
  match prop with
  | Lit lit -> lit_is_v_eq_lit lit v
  | Iff (Lit { x = AVar var; _ }, Lit b) when String.equal var.x v -> Some b
  | Iff (Lit b, Lit { x = AVar var; _ }) when String.equal var.x v -> Some b
  | _ -> None

let smart_sigma (qv, xprop) prop =
  match qv.ty with
  | Nt.Ty_unit -> smart_add_to xprop prop
  | _ -> (
      match prop_is_v_eq_lit xprop qv.x with
      | Some lit -> subst_prop_instance qv.x lit.x prop
      | None ->
          let body = smart_add_to xprop prop in
          let fv = fv_prop body in
          if List.exists (fun y -> String.equal qv.x y.x) fv then
            Exists { qv; body }
          else body)

let smart_pi (qv, xprop) prop =
  match qv.ty with
  | Nt.Ty_unit -> smart_implies xprop prop
  | _ -> (
      match prop_is_v_eq_lit xprop qv.x with
      | Some lit -> subst_prop_instance qv.x lit.x prop
      | None ->
          let body = smart_implies xprop prop in
          let fv = fv_prop body in
          if List.exists (fun y -> String.equal qv.x y.x) fv then
            Forall { qv; body }
          else body)

(* let template_eq = "forall v == " *)

let mk_prop_var_eq_c nty (id, c) =
  match c with
  | U -> mk_true
  | _ ->
      let lit =
        mk_lit_eq nty
          (mk_typed_lit_by_id id #: nty, mk_typed_lit_by_const c #: nty)
      in
      Lit lit #: nty

let mk_prop_var_eq_var nty (id, id') =
  match nty with
  | Nt.Ty_unit -> mk_true
  | _ ->
      let lit =
        mk_lit_eq nty
          (mk_typed_lit_by_id id #: nty, mk_typed_lit_by_id id' #: nty)
      in
      Lit lit #: nty

let prop_get_mp prop =
  let rec aux prop_e =
    match prop_e with
    | Lit lit -> (
        match typed_lit_get_mp lit with Some mp -> [ mp ] | None -> [])
    | Implies (p1, p2) -> aux p1 @ aux p2
    | Ite (p1, p2, p3) -> aux p1 @ aux p2 @ aux p3
    | Not p1 -> aux p1
    | And ps -> List.concat (List.map aux ps)
    | Or ps -> List.concat (List.map aux ps)
    | Iff (p1, p2) -> aux p1 @ aux p2
    | Forall { body; _ } -> aux body
    | Exists { body; _ } -> aux body
  in
  List.slow_rm_dup String.equal @@ aux prop

let apply_pi_prop (p : t prop) (lit : (t, t lit) typed) =
  match p with
  | Forall { qv; body } ->
      if Nt.eq qv.ty lit.ty then subst_prop_instance qv.x lit.x body
      else _failatwith __FILE__ __LINE__ "die"
  | _ -> _failatwith __FILE__ __LINE__ "die"

let mapply_pi_prop (p : t prop) lits = List.fold_left apply_pi_prop p lits
