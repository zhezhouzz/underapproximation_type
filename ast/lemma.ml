include Quantified.EPR
module P = Autov.Prop

let to_prop { qvs = eqvs; qbody = { qvs = uqvs; qbody = prop } } =
  let open P in
  List.fold_right
    (fun qv prop -> Exists (Typed.Ntyped.to_q_typed qv, prop))
    eqvs
    (List.fold_right
       (fun qv prop -> Forall (Typed.Ntyped.to_q_typed qv, prop))
       uqvs prop)
