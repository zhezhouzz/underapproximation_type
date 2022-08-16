module F (Type : Type.T) = struct
  type 'a typed = { x : 'a; ty : Type.t } [@@deriving sexp]
end

module Ntyped = F (Normalty.T)
