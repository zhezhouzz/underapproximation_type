open Mtyped
module Nt = Normalty.Frontend
open Sugar
open Zzdatatype.Datatype

let layout_typectx (layout : 'a -> string) ctx : string =
  match ctx with
  | Typectx.Typectx l ->
      List.split_by_comma (fun { x; ty } -> spf "%s:%s" x (layout ty)) l
