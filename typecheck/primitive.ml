open Zzdatatype.Datatype
module NT = Languages.Normalty

let tab =
  let open Languages.Normalty in
  [
    ("::", Ty_arrow (Ty_int, Ty_arrow (Ty_list Ty_int, Ty_list Ty_int)));
    ("[]", Ty_list Ty_int);
  ]

let m = StrMap.from_kv_list tab

let get_primitive_ty name =
  StrMap.find (Sugar.spf "cannot find primitive type of %s" name) m name

module P = Autov.Prop
module T = Autov.Smtty

let over_tab =
  let open Languages.Overty in
  let mk_int_var name = P.{ ty = Some T.Int; x = name } in
  let u, _, _ = Sugar.map3 mk_int_var ("_u", "_w", "_z") in
  [
    ( "::",
      let basename = "_nu" in
      let nu = P.{ ty = Some T.Int; x = basename } in
      let prop h t =
        P.(
          Forall
            ( u,
              Iff
                ( Or
                    [
                      MethodPred ("mem", [ t; u ]); MethodPred ("==", [ h; u ]);
                    ],
                  MethodPred ("mem", [ nu; u ]) ) ))
      in
      make_arrow "_h" (make_basic_top NT.Ty_int) (fun h ->
          make_arrow "_t"
            (make_basic_top NT.(Ty_list Ty_int))
            (fun t ->
              OverTy_base
                { basename; normalty = NT.(Ty_list Ty_int); prop = prop h t }))
    );
    ( "[]",
      let basename = "_nu" in
      let normalty = get_primitive_ty "[]" in
      let nu = P.{ ty = Some T.Int; x = basename } in
      let prop = P.(Forall (u, Not (MethodPred ("mem", [ nu; u ])))) in
      OverTy_base { basename; normalty; prop } );
  ]

let over_m = StrMap.from_kv_list over_tab

let over_get_primitive_ty name =
  StrMap.find
    (Sugar.spf "cannot find over primitive type of %s" name)
    over_m name
