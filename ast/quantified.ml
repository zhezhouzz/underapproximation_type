module F (Type : Type.T) (Qb : Quantifiable.T) = struct
  open Sexplib.Std

  type id = Strid.T.t [@@deriving sexp]

  open Typed.F (Type)

  type t = { qvs : id typed list; qbody : Qb.t } [@@deriving sexp]

  let fresh_name_from_old name = Rename.unique_with_prefix "q" name

  (* open Sugar *)

  let qids (x : t) = List.map (fun x -> x.x) x.qvs

  open Zzdatatype.Datatype

  let typed_id_eq x y = Type.eq x.ty y.ty && String.equal x.x y.x
  let without_qv qbody = { qvs = []; qbody }
  let map f { qvs; qbody } = { qvs; qbody = f qbody }

  let check_close { qvs; qbody } =
    let fv = Qb.fv qbody in
    match
      Zzdatatype.Datatype.List.substract String.equal
        (List.map (fun x -> x.x) qvs)
        fv
    with
    | [] -> true
    | _ -> false
end

module Qunderty = F (Normalty.T) (Underty.T)

module EProp = struct
  include F (Normalty.T) (Autov.Prop)
  open Zzdatatype.Datatype
  module P = Autov.Prop
  open Typed.Ntyped

  let var_space { qvs; qbody } =
    List.slow_rm_dup String.equal
    @@ List.map (fun x -> x.x) qvs
    @ P.var_space qbody

  let eq a b = List.eq eq a.qvs b.qvs && P.eq a.qbody b.qbody

  let subst_id { qvs; qbody } x z =
    if List.exists (fun y -> String.equal x y.x) qvs then { qvs; qbody }
    else { qvs; qbody = P.subst_id qbody x z }

  let fv { qvs; qbody } =
    List.filter (fun x -> not @@ List.exists (fun y -> String.equal x y.x) qvs)
    @@ P.fv qbody
end

module EPR = F (Normalty.T) (EProp)
