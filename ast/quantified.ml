module F (Type : Type.T) (Qb : Quantifiable.T) = struct
  open Sexplib.Std

  type id = Strid.T.t [@@deriving sexp]

  open Typed.F (Type)

  type t = { uqvs : id typed list; eqvs : id typed list; qbody : Qb.t }
  [@@deriving sexp]

  let fresh_name_from_old name = Rename.unique_with_prefix "q" name

  open Sugar

  let uqids (x : t) = List.map (fun x -> x.x) x.uqvs
  let eqids (x : t) = List.map (fun x -> x.x) x.eqvs

  open Zzdatatype.Datatype

  let unify_uqvs (uqvs1, qbody1, subst) target =
    let uqvs =
      if List.length target.uqvs >= List.length uqvs1 then target.uqvs
      else
        let sub =
          List.sublist uqvs1 ~start_included:(List.length target.uqvs)
            ~end_excluded:(List.length uqvs1)
        in
        (* let _ = Printf.printf "len: %i" @@ List.length sub in *)
        target.uqvs @ sub
    in
    (* let () = *)
    (*   Printf.printf "uqvs1: %i, target.uqvs:%i, uqvs:%i\n" (List.length uqvs1) *)
    (*     (List.length target.uqvs) (List.length uqvs) *)
    (* in *)
    let qbody1 =
      List.fold_left (fun qbody (x, y) -> subst qbody x.x y.x) qbody1
      @@ List.combine uqvs1
      @@ List.sublist uqvs ~start_included:0 ~end_excluded:(List.length uqvs1)
    in
    (uqvs, qbody1)

  let unify (uqvs, eqvs, (qbody : 'a), (subst : 'a -> string -> string -> 'a))
      (target : t) =
    (* let var_space = uqids target @ eqids target @ Qb.var_space target.qbody in *)
    let uqvs, qbody = unify_uqvs (uqvs, qbody, subst) target in
    let to_unique ({ ty; x }, qbody) =
      (* if List.exists (String.equal x) var_space then *)
      if true then
        let x' = fresh_name_from_old x in
        ({ ty; x = x' }, subst qbody x x')
      else ({ ty; x }, qbody)
    in
    let eqvs, qbody =
      List.fold_left
        (fun (eqvs, qbody) x ->
          let x, qbody = to_unique (x, qbody) in
          (eqvs @ [ x ], qbody))
        ([], qbody) eqvs
    in
    (* let uqvs, qbody = *)
    (*   List.fold_left *)
    (*     (fun (eqvs, qbody) x -> *)
    (*       let x, qbody = to_unique (x, qbody) in *)
    (*       (eqvs @ [ x ], qbody)) *)
    (*     ([], qbody) uqvs *)
    (* in *)
    (* let uqvs = target.uqvs @ uqvs in *)
    let eqvs = target.eqvs @ eqvs in
    (qbody, { uqvs; eqvs; qbody = target.qbody })

  let typed_id_eq x y = Type.eq x.ty y.ty && String.equal x.x y.x

  let subtract_qv qvs qvs' =
    let rec aux = function
      | qvs, [] -> qvs
      | [], _ -> _failatwith __FILE__ __LINE__ ""
      | qv1 :: qvs1, qv2 :: qvs2 when typed_id_eq qv1 qv2 -> aux (qvs1, qvs2)
      | _, _ -> _failatwith __FILE__ __LINE__ ""
    in
    aux (qvs, qvs')

  let subtract x y = (subtract_qv x.uqvs y.uqvs, subtract_qv x.eqvs y.eqvs)
  let without_qv qbody = { uqvs = []; eqvs = []; qbody }
  let map f { uqvs; eqvs; qbody } = { uqvs; eqvs; qbody = f qbody }

  let check_close { uqvs; eqvs; qbody } =
    let fv = Qb.fv qbody in
    match
      Zzdatatype.Datatype.List.substract String.equal
        (List.map (fun x -> x.x) uqvs @ List.map (fun x -> x.x) eqvs)
        fv
    with
    | [] -> true
    | _ -> false
end

module Qunderty = F (Normalty.T) (Underty.T)
