module F (Type : Type.T) = struct
  type 'a typed = { x : 'a; ty : Type.t } [@@deriving sexp]

  let map (f : 'a -> 'b) { x; ty } = { x = f x; ty }

  let rename_with_vars (vars, prop) =
    List.fold_right
      (fun qv (vars, prop) ->
        let qv' = { x = Rename.unique qv.x; ty = qv.ty } in
        (qv' :: vars, Autov.Prop.subst_id prop qv.x qv'.x))
      vars ([], prop)

  let unify_to_vars (vars, prop) vars' =
    let rec aux prop = function
      | [], _ -> prop
      | h :: t, h' :: t' -> aux (Autov.Prop.subst_id prop h.x h'.x) (t, t')
      | _ -> failwith "die"
    in
    aux prop (vars, vars')
end

module Ntyped = struct
  include F (NT)

  let eq a b = String.equal a.x b.x && NT.eq a.ty b.ty
  let to_q_typed { ty; x } = Autov.Prop.{ ty = NT.to_smtty ty; x }
end

module Ptyped = struct
  include F (Autov.Prop)
end

module SMTtyped = struct
  include F (Autov.Smtty)

  let eq a b = String.equal a.x b.x && Autov.Smtty.eq a.ty b.ty
end

module NNtyped = struct
  include F (NotatedT)

  let eq a b = String.equal a.x b.x && NotatedT.eq a.ty b.ty
  let to_q_typed { ty; x } = Autov.Prop.{ ty = NT.to_smtty @@ snd ty; x }
end
