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
