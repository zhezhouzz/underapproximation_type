open Normalty.Ast
include Quantified.F (Ntyped) (Underty.T)

(* open Sugar *)
open Zzdatatype.Datatype

let eq a b =
  (List.eq Ntyped.typed_eq) a.qvs b.qvs && Underty.T.strict_eq a.qbody b.qbody
