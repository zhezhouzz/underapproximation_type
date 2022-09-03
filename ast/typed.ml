module F (Type : Type.T) = struct
  type 'a typed = { x : 'a; ty : Type.t } [@@deriving sexp]

  let map (f : 'a -> 'b) { x; ty } = { x = f x; ty }
end

module Ntyped = struct
  include F (Normalty.T)

  let eq a b = String.equal a.x b.x && Normalty.T.eq a.ty b.ty
  let to_q_typed { ty; x } = Autov.Prop.{ ty = Normalty.T.to_smtty ty; x }
end

module SMTtyped = struct
  include F (Autov.Smtty)

  let eq a b = String.equal a.x b.x && Autov.Smtty.eq a.ty b.ty
end

module NNtyped = struct
  include F (Normalty.NotatedT)

  let eq a b = String.equal a.x b.x && Normalty.NotatedT.eq a.ty b.ty

  let to_q_typed { ty; x } =
    Autov.Prop.{ ty = Normalty.T.to_smtty @@ snd ty; x }
end
