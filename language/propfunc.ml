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

let unfold_or prop =
  let rec aux = function
    | [] -> []
    | Or l :: l' -> aux (l @ l')
    | prop :: l' -> prop :: aux l'
  in
  let l = aux prop in
  List.slow_rm_dup eq_prop l

let remove_id_eq_lit lit =
  match lit.x with
  | AAppOp (op, [ a; b ]) when String.equal op.x "==" ->
      if eq_lit a.x b.x then (AC (B true)) #: Nt.Ty_bool else lit
  | _ -> lit

let rec smart_and_ l =
  let l = unfold_and l in
  if List.exists is_false l then mk_false
  else
    match List.filter (fun p -> not (is_true p)) l with
    | [] -> mk_true
    | [ x ] -> x
    | l -> And l

and simplify_prop prop =
  let rec aux prop =
    match prop with
    | Lit lit -> Lit (remove_id_eq_lit lit)
    | Implies (p1, p2) -> Implies (aux p1, aux p2)
    | Ite (p1, p2, p3) -> Ite (aux p1, aux p2, aux p3)
    | Not p1 -> Not (aux p1)
    | And ps -> smart_and_ (List.map aux ps)
    | Or ps -> smart_or_ (List.map aux ps)
    | Iff (p1, p2) when eq_prop p1 p2 -> mk_true
    | Iff (p1, p2) -> Iff (aux p1, aux p2)
    | Forall { body; qv } ->
        let body = aux body in
        if List.exists (String.equal qv.x) @@ fv_prop_id body then
          Forall { body; qv }
        else body
    | Exists { body; qv } ->
        let body = aux body in
        if List.exists (String.equal qv.x) @@ fv_prop_id body then
          Exists { body; qv }
        else body
  in
  aux prop

and smart_or_ l =
  let l = unfold_or l in
  if List.exists is_true l then mk_true
  else
    match List.filter (fun p -> not (is_false p)) l with
    | [] -> mk_false
    | [ x ] -> x
    | l -> Or l

let smart_and l = simplify_prop (And l)
let smart_or l = simplify_prop (Or l)

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
  | _ ->
      let body = smart_add_to xprop prop in
      let body =
        match body with
        | And l -> (
            let l' = List.filter_map (fun p -> prop_is_v_eq_lit p qv.x) l in
            match l' with
            | [ lit ] -> subst_prop_instance qv.x lit.x body
            | _ -> body)
        | _ -> body
      in
      let fv = fv_prop body in
      if List.exists (fun y -> String.equal qv.x y.x) fv then
        Exists { qv; body }
      else body

(* ( *)
(*   match prop_is_v_eq_lit xprop qv.x with *)
(*   | Some lit -> subst_prop_instance qv.x lit.x prop *)
(*   | None -> *)
(*     (match prop_is_v_eq_lit prop qv.x with *)
(*      | Some lit -> *)
(*        subst_prop_instance qv.x lit.x prop *)
(*      | None -> *)
(*       let body = smart_add_to xprop prop in *)
(*       let fv = fv_prop body in *)
(*       if List.exists (fun y -> String.equal qv.x y.x) fv then *)
(*         Exists { qv; body } *)
(*       else body)) *)

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

(* let ho_subst_prop name params (x : t prop) (lit : t lit) = *)
(*   match lit with *)
(*   | AAppOp (y, args) when String.equal y.x name -> *)
(*       let params = _safe_combine __FILE__ __LINE__ params args in *)
(*       let x' = *)
(*         List.fold_left *)
(*           (fun res (x, lit) -> subst_lit_instance x lit.x res) *)
(*           x params *)
(*       in *)
(*       Some x' *)
(*   | _ -> None *)

let destruct_forall_prop (p : t prop) =
  let rec aux = function
    | Forall { body; qv } ->
        let args, prop = aux body in
        (qv.x :: args, prop)
    | _ as prop -> ([], prop)
  in
  aux p

type prop_var = { params : string list; body : t prop }

let ho_subst_prop name prop_var (prop : t prop) =
  let rec aux prop_e =
    match prop_e with
    | Lit lit -> (
        match lit.x with
        | AAppOp (y, args) when String.equal y.x name ->
            let params = _safe_combine __FILE__ __LINE__ prop_var.params args in
            let p =
              List.fold_left
                (fun res (x, lit) -> subst_prop_instance x lit.x res)
                prop_var.body params
            in
            p
        | _ -> Lit lit)
    | Implies (p1, p2) -> Implies (aux p1, aux p2)
    | Ite (p1, p2, p3) -> Ite (aux p1, aux p2, aux p3)
    | Not p1 -> Not (aux p1)
    | And ps -> And (List.map aux ps)
    | Or ps -> Or (List.map aux ps)
    | Iff (p1, p2) -> Iff (aux p1, aux p2)
    | Forall { body; qv } -> Forall { body = aux body; qv }
    | Exists { body; qv } -> Exists { body = aux body; qv }
  in
  simplify_prop @@ aux prop
