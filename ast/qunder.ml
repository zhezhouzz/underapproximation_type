include Quantified.F (Normalty.T) (Underty.T)

(* open Sugar *)
open Zzdatatype.Datatype

let eq a b =
  (List.eq Typed.Ntyped.eq) a.qvs b.qvs && Underty.T.strict_eq a.qbody b.qbody
