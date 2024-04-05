include Normalty.Connective
include Mtyped
include Constant
include Op
include Lit
include Prop
include Cty
include Rty
include Typectx
include Raw_term
include Term
include Constructor_declaration
include Item

module Nt = struct
  include Normalty.Ntyped
  include Normalty.Frontend
end
