open Sexplib.Std
open Mtyped
module Nt = Normalty.Ntyped
open Prop

type 't cty = Cty of { nty : Nt.t; phi : 't prop } [@@deriving sexp]
