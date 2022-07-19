module Type = Languages.Normalty
open Type
open Zzdatatype.Datatype

let tab =
  [
    ("::", Ty_arrow (Ty_int, Ty_arrow (Ty_list Ty_int, Ty_list Ty_int)));
    ("[]", Ty_list Ty_int);
  ]

let m = StrMap.from_kv_list tab

let get_primitive_ty name =
  StrMap.find (Sugar.spf "cannot find primitive type of %s" name) m name
