open Zzdatatype.Datatype

let get_primitive_ty m name =
  StrMap.find (Sugar.spf "cannot find primitive type of %s" name) m name

let get_primitive_normal_ty = get_primitive_ty Normalprim.m
let get_primitive_over_ty = get_primitive_ty Overprim.m
let get_primitive_under_ty = get_primitive_ty Underprim.m
let get_primitive_rev_under_ty = get_primitive_ty Underprim.rev_m
