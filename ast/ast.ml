module Op = Op.T
module Value = Value

(* include Normalty.Ast *)
module NNormalty = Normalty.Notatedtyped

module NT = struct
  include T

  let layout = Normalty.Frontend.layout
end

module Otyped = Overty.Otyped
module OT = Overty.T
module UT = Underty.T
module MMT = Underty.MMT
module Ntyped = Normalty.Ntyped
module NNtyped = Normalty.Notatedtyped
module NL = Anormal.NormalAnormal
module OL = Anormal.OverAnormal
module UL = Anormal.UnderAnormal
module Termlang = Termlang.T
module Signat = Modu.Signat
module Struc = Modu.Struc
module StrucNA = Modu.StrucNA
module StrucOA = Modu.StrucOA
module MultiUnderTypectx = Typectx.MultiUnderTypectx
module NSimpleTypectx = Simpletypectx.NSimpleTypectx
module SMTSimpleTypectx = Simpletypectx.SMTSimpleTypectx
module UTSimpleTypectx = Simpletypectx.UTSimpleTypectx
module OverTypectx = Simpletypectx.OverTypectx
module Typedec = Type_dec.T
module Lemma = Lemma
module MustMayTypectx = Simpletypectx.MustMayTypectx
