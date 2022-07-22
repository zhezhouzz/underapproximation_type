open Zzdatatype.Datatype
module NT = Languages.Normalty
module P = Autov.Prop
module T = Autov.Smtty

let over_tab =
  let open Languages.Overty in
  let mk_int_var name = P.{ ty = T.Int; x = name } in
  let u, _, _ = Sugar.map3 mk_int_var ("_u", "_w", "_z") in
  [
    ( "::",
      let basename = "_nu" in
      let nu = P.{ ty = T.Int; x = basename } in
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
      let normalty = NT.(Ty_list Ty_int) in
      let nu = P.{ ty = T.Int; x = basename } in
      let prop = P.(Forall (u, Not (MethodPred ("mem", [ nu; u ])))) in
      OverTy_base { basename; normalty; prop } );
    ( "<",
      let basename = "_nu" in
      let normalty = NT.Ty_bool in
      let nu = P.{ ty = T.Bool; x = basename } in
      make_arrow "_a" (make_basic_top NT.Ty_int) (fun a ->
          make_arrow "_b"
            (make_basic_top NT.(Ty_int))
            (fun b ->
              OverTy_base
                {
                  basename;
                  normalty;
                  prop = P.(Iff (Var nu, MethodPred ("<", [ a; b ])));
                })) );
  ]

let m = StrMap.from_kv_list over_tab
