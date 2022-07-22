open Zzdatatype.Datatype
module NT = Languages.Normalty

let tab =
  let open Languages.Normalty in
  [
    ("::", Ty_arrow (Ty_int, Ty_arrow (Ty_list Ty_int, Ty_list Ty_int)));
    ("[]", Ty_list Ty_int);
    ("<", Ty_arrow (Ty_int, Ty_arrow (Ty_int, Ty_bool)));
  ]

let m = StrMap.from_kv_list tab

let get_primitive_ty name =
  StrMap.find (Sugar.spf "cannot find primitive type of %s" name) m name
