module T = struct
  module P = Autov.Prop
  module T = Autov.Smtty
  module NT = Normalty.Ast.NT
  module NTyped = Normalty.Ast.Ntyped
  open Sexplib.Std
  open Sugar

  type id = string [@@deriving sexp]
  type normalty = NT.t [@@deriving sexp]

  open NTyped

  (* invariant: the prop here should be existensial quantified. *)
  (* the tuple now is the dependent tuple *)
  type ot = { basename : id; normalty : normalty; prop : P.t } [@@deriving sexp]

  type t =
    | UnderTy_base of { basename : id; normalty : normalty; prop : P.t }
    | UnderTy_under_arrow of { argty : t; retty : t }
    | UnderTy_over_arrow of { argname : id; argty : ot; retty : t }
    | UnderTy_tuple of t list
  [@@deriving sexp]

  open Zzdatatype.Datatype

  let ot_to_ut { basename; normalty; prop } =
    UnderTy_base { basename; normalty; prop }

  let default_v_name = "v"

  let make_basic basename normalty prop =
    UnderTy_base
      { basename; normalty; prop = prop { x = basename; ty = normalty } }

  let make_basic_from_const_int (n : int) =
    make_basic default_v_name NT.Ty_int (fun nu ->
        P.(mk_lit_eq_lit (AVar nu) (ACint n)))

  let make_basic_from_const_bool (b : bool) =
    make_basic default_v_name NT.Ty_bool
      P.(
        fun nu ->
          let nu = Lit (AVar nu) in
          match b with true -> nu | false -> Not nu)

  let make_basic_from_prop nt propf = make_basic default_v_name nt propf

  let ot_make_basic_from_prop nt propf =
    match make_basic default_v_name nt propf with
    | UnderTy_base { basename; normalty; prop } -> { basename; normalty; prop }
    | _ -> _failatwith __FILE__ __LINE__ "die"

  let make_basic_from_eq_var x =
    make_basic_from_prop x.ty (fun v ->
        P.(MethodPred ("==", [ AVar v; AVar x ])))

  let make_basic_top normalty =
    make_basic default_v_name normalty (fun _ -> P.mk_true)

  let make_basic_bot normalty =
    make_basic default_v_name normalty (fun _ -> P.mk_false)

  let t_to_exn_type t =
    let rec aux = function
      | UnderTy_base { basename; normalty; _ } ->
          UnderTy_base { basename; normalty; prop = P.mk_false }
      | UnderTy_tuple ts -> UnderTy_tuple (List.map aux ts)
      | UnderTy_over_arrow { argname; argty; retty } ->
          UnderTy_over_arrow { argname; argty; retty = aux retty }
      | UnderTy_under_arrow { argty; retty } ->
          UnderTy_under_arrow { argty; retty = aux retty }
    in
    aux t

  let nt_to_exn_type t =
    let rec aux t =
      match t with
      | Ty_unknown | Ty_var _ -> _failatwith __FILE__ __LINE__ ""
      | Ty_unit | Ty_int | Ty_bool | Ty_list _ | Ty_tree _ | Ty_constructor _ ->
          make_basic_bot t
      | Ty_arrow (t1, t2) ->
          UnderTy_under_arrow { argty = aux t1; retty = aux t2 }
      | Ty_tuple ts -> UnderTy_tuple (List.map aux ts)
    in
    aux t

  let var_space t =
    let add xs s = List.fold_left (fun s x -> StrMap.add x () s) s xs in
    let aux_ot s { basename; prop; _ } =
      let s = add (P.var_space prop) s in
      add [ basename ] s
    in
    let rec aux s = function
      | UnderTy_base { basename; prop; _ } ->
          let s = add (P.var_space prop) s in
          add [ basename ] s
      | UnderTy_tuple ts -> List.fold_left aux s ts
      | UnderTy_over_arrow { argname; retty; argty } ->
          let s = aux_ot s argty in
          add (argname :: (StrMap.to_key_list @@ aux StrMap.empty retty)) s
      | UnderTy_under_arrow { argty; retty } ->
          let space =
            StrMap.to_key_list @@ aux (aux StrMap.empty argty) retty
          in
          add space s
    in
    StrMap.to_key_list @@ aux StrMap.empty t

  let rec erase = function
    | UnderTy_base { normalty; _ } -> normalty
    | UnderTy_under_arrow { argty; retty; _ } ->
        NT.Ty_arrow (erase argty, erase retty)
    | UnderTy_over_arrow { argty; retty; _ } ->
        NT.Ty_arrow (argty.normalty, erase retty)
    | UnderTy_tuple ts -> NT.Ty_tuple (List.map erase ts)

  let ot_subst_id t x y =
    match t with
    | { basename; normalty; prop } ->
        if String.equal basename x then t
        else { basename; normalty; prop = P.subst_id prop x y }

  let subst_id t x y =
    let rec aux t =
      match t with
      | UnderTy_base { basename; normalty; prop } ->
          if String.equal basename x then t
          else UnderTy_base { basename; normalty; prop = P.subst_id prop x y }
      | UnderTy_over_arrow { argname; argty; retty } ->
          let argty = ot_subst_id argty x y in
          let retty =
            if List.exists (String.equal x) [ argname ] then retty
            else aux retty
          in
          UnderTy_over_arrow { argname; argty; retty }
      | UnderTy_under_arrow { argty; retty } ->
          UnderTy_under_arrow { argty = aux argty; retty = aux retty }
      | UnderTy_tuple ts -> UnderTy_tuple (List.map aux ts)
    in
    aux t

  let assume_base_destruct_opt = function
    | UnderTy_base { basename; normalty; prop } ->
        Some (basename, normalty, prop)
    | _ -> None

  let assume_base file line = function
    | UnderTy_base { basename; normalty; prop } -> (basename, normalty, prop)
    | _ -> _failatwith file line ""

  let assume_base_subst_id file line name = function
    | UnderTy_base { basename; normalty; prop } ->
        UnderTy_base
          { basename = name; normalty; prop = P.subst_id prop basename name }
    | _ -> _failatwith file line ""

  let assume_tuple file line = function
    | UnderTy_tuple tys -> tys
    | _ -> _failatwith file line ""

  (* let assume_arrow_destruct_ *)

  (* let mk_int_id name = P.{ ty = T.Int; x = name } *)

  let make_over_arrow argname normalty argtyf rettyf =
    let id = { ty = normalty; x = argname } in
    UnderTy_over_arrow
      { argname; argty = argtyf argname normalty; retty = rettyf id }

  let is_base_type = function UnderTy_base _ -> true | _ -> false

  let ot_strict_eq { basename = basename1; normalty = normalty1; prop = prop1 }
      { basename = basename2; normalty = normalty2; prop = prop2 } =
    String.equal basename1 basename2
    && NT.eq normalty1 normalty2 && P.strict_eq prop1 prop2

  let strict_eq t1 t2 =
    let rec aux (t1, t2) =
      match (t1, t2) with
      | ( UnderTy_base
            { basename = basename1; normalty = normalty1; prop = prop1 },
          UnderTy_base
            { basename = basename2; normalty = normalty2; prop = prop2 } ) ->
          String.equal basename1 basename2
          && NT.eq normalty1 normalty2 && P.strict_eq prop1 prop2
      | UnderTy_tuple ts1, UnderTy_tuple ts2 ->
          List.for_all (fun (aty, bty) -> aux (aty, bty))
          @@ _safe_combine __FILE__ __LINE__ ts1 ts2
      | ( UnderTy_over_arrow
            { argname = argname1; argty = argty1; retty = retty1 },
          UnderTy_over_arrow
            { argname = argname2; argty = argty2; retty = retty2 } ) ->
          String.equal argname1 argname2
          && ot_strict_eq argty1 argty2
          && aux (retty1, retty2)
      | ( UnderTy_under_arrow { argty = argty1; retty = retty1 },
          UnderTy_under_arrow { argty = argty2; retty = retty2 } ) ->
          aux (argty1, argty2) && aux (retty1, retty2)
      | _, _ -> false
    in
    aux (t1, t2)

  let eq a b = strict_eq a b

  let strict_eq_besides_retty t1 t2 =
    let rec aux (t1, t2) =
      match (t1, t2) with
      | UnderTy_base _, UnderTy_base _ -> true
      | UnderTy_tuple ts1, UnderTy_tuple ts2 ->
          List.for_all aux @@ _safe_combine __FILE__ __LINE__ ts1 ts2
      | ( UnderTy_over_arrow
            { argname = argname1; argty = argty1; retty = retty1 },
          UnderTy_over_arrow
            { argname = argname2; argty = argty2; retty = retty2 } ) ->
          String.equal argname1 argname2
          && ot_strict_eq argty1 argty2
          && aux (retty1, retty2)
      | ( UnderTy_under_arrow { argty = argty1; retty = retty1 },
          UnderTy_under_arrow { argty = argty2; retty = retty2 } ) ->
          strict_eq argty1 argty2 && aux (retty1, retty2)
      | _, _ -> false
    in
    aux (t1, t2)

  let _map_base_type f { basename; normalty; prop } =
    let x = { x = basename; ty = normalty } in
    { basename; normalty; prop = f x prop }

  let map_base_type f = function
    | UnderTy_base { basename; normalty; prop } ->
        let { basename; normalty; prop } =
          _map_base_type f { basename; normalty; prop }
        in
        UnderTy_base { basename; normalty; prop }
    | _ -> _failatwith __FILE__ __LINE__ ""

  let _zip_base_type f
      { basename = basename1; normalty = normalty1; prop = prop1 }
      { basename = basename2; normalty = normalty2; prop = prop2 } =
    let normalty =
      _check_equality __FILE__ __LINE__ NT.eq normalty1 normalty2
    in
    let prop2 =
      if String.equal basename1 basename2 then prop2
      else P.subst_id prop2 basename2 basename1
    in
    { basename = basename1; normalty; prop = f prop1 prop2 }

  let ot_zip_base_type f t1 t2 = _zip_base_type f t1 t2

  let zip_base_type f = function
    | ( UnderTy_base { basename = basename1; normalty = normalty1; prop = prop1 },
        UnderTy_base
          { basename = basename2; normalty = normalty2; prop = prop2 } ) ->
        let { basename; normalty; prop } =
          _zip_base_type f
            { basename = basename1; normalty = normalty1; prop = prop1 }
            { basename = basename2; normalty = normalty2; prop = prop2 }
        in
        UnderTy_base { basename; normalty; prop }
    | _, _ -> _failatwith __FILE__ __LINE__ ""

  let map_in_retty (f : t -> t) t =
    let rec aux t =
      match t with
      | UnderTy_base _ -> f t
      | UnderTy_tuple ts -> UnderTy_tuple (List.map aux ts)
      | UnderTy_over_arrow { argname; argty; retty } ->
          UnderTy_over_arrow { argname; argty; retty = aux retty }
      | UnderTy_under_arrow { argty; retty } ->
          UnderTy_under_arrow { argty; retty = aux retty }
    in
    aux t

  let zip f t1 t2 =
    if not (strict_eq_besides_retty t1 t2) then _failatwith __FILE__ __LINE__ ""
    else
      map_in_retty
        (fun retty1 ->
          map_in_retty (fun retty2 -> zip_base_type f (retty1, retty2)) t2)
        t1

  let disjunct =
    zip (fun p1 p2 -> P.disjunct_tope_uprop __FILE__ __LINE__ [ p1; p2 ])

  let conjunct =
    zip (fun p1 p2 -> P.conjunct_tope_uprop __FILE__ __LINE__ [ p1; p2 ])

  let disjunct_list ts =
    match ts with
    | [] -> _failatwith __FILE__ __LINE__ "disjunct no types"
    | [ t ] -> t
    | h :: t -> List.fold_left disjunct h t

  let conjunct_list ts =
    match ts with
    | [] -> _failatwith __FILE__ __LINE__ "conjunct no types"
    | [ t ] -> t
    | h :: t -> List.fold_left conjunct h t

  let ot_fv { basename; prop; _ } =
    let fv = Autov.prop_fv prop in
    List.filter (fun x -> not @@ String.equal basename x) fv

  let fv bodyt =
    let remove name = List.filter (fun x -> not @@ String.equal x name) in
    let rec aux = function
      | UnderTy_base { basename; prop; _ } ->
          let fv = Autov.prop_fv prop in
          List.filter (fun x -> not @@ String.equal basename x) fv
      | UnderTy_tuple ts ->
          let rec loop ts =
            match ts with [] -> [] | ty :: ts -> aux ty @ loop ts
          in
          loop ts
      | UnderTy_under_arrow { argty; retty } ->
          let fv_retty = aux retty in
          let fv_argty = aux argty in
          Zzdatatype.Datatype.List.slow_rm_dup String.equal (fv_retty @ fv_argty)
      | UnderTy_over_arrow { argname; retty; argty } ->
          let fv_argty = ot_fv argty in
          let fv_retty = remove argname (aux retty) in
          Zzdatatype.Datatype.List.slow_rm_dup String.equal (fv_retty @ fv_argty)
    in
    aux bodyt

  let is_fv_in name ty = List.exists (String.equal name) @@ fv ty

  let eqv_to_bodyt { ty; x } =
    UnderTy_base { basename = x; normalty = ty; prop = P.mk_true }

  let join_tuple_t t = function
    | UnderTy_tuple ts -> UnderTy_tuple (ts @ [ t ])
    | _ -> _failatwith __FILE__ __LINE__ ""

  let modify_retty f t =
    map_in_retty (fun retty -> map_base_type (fun x prop -> f x prop) retty) t

  let retty_add_bool_eq t x b =
    let open P in
    modify_retty
      (fun _ prop ->
        let prop' = MethodPred ("==", [ AVar { x; ty = Ty_bool }; ACbool b ]) in
        P.conjunct_tope_uprop __FILE__ __LINE__ [ prop; prop' ])
      t

  let extract_refinement_from_base file line (id, idty) =
    let bname, nt, prop = assume_base file line idty in
    ({ x = id; ty = nt }, P.subst_id prop bname id)

  let retty_add_ex_uprop_with_option if_allow_drop (id, idty) t =
    let open P in
    modify_retty
      (fun _ prop ->
        (* let fv_res = *)
        (*   List.map (fun x -> (id, x, String.equal id x)) @@ fv prop *)
        (* in *)
        (* let () = *)
        (*   Pp.printf "%s\n" *)
        (*   @@ List.split_by "\n" *)
        (*        (fun (id, x, b) -> *)
        (*          spf "[allow_drop? %b]%s is equal to fv (%s): %b\n" *)
        (*            if_allow_drop id x b) *)
        (*        fv_res *)
        (* in *)
        if if_allow_drop && not (List.exists (String.equal id) @@ fv prop) then
          prop
        else
          let x, xprop =
            extract_refinement_from_base __FILE__ __LINE__ (id, idty)
          in
          let xeqvs, xprop = P.assume_tope_uprop __FILE__ __LINE__ xprop in
          let eqvs, prop = P.assume_tope_uprop __FILE__ __LINE__ prop in
          let prop =
            P.conjunct_eprop_to_right_ (x :: xeqvs, xprop) (eqvs, prop)
          in
          prop)
      t

  let retty_add_ex_uprop_drop_independent (id, idty) t =
    retty_add_ex_uprop_with_option true (id, idty) t

  let retty_add_ex_uprop_always_add (id, idty) t =
    retty_add_ex_uprop_with_option false (id, idty) t

  (* let reduce_inv_type_by_name t name = *)
  (*   let rec aux ghost_t t = *)
  (*     match t with *)
  (*     | UnderTy_base { basename; normalty; prop } -> ( *)
  (*         match ghost_t with *)
  (*         | None -> _failatwith __FILE__ __LINE__ "" *)
  (*         | Some (x, xprop) -> *)
  (*             let xeqvs, xprop = P.assume_tope_uprop __FILE__ __LINE__ xprop in *)
  (*             let eqvs, prop = P.assume_tope_uprop __FILE__ __LINE__ prop in *)
  (*             let prop = *)
  (*               P.conjunct_eprop_to_right_ (x :: xeqvs, xprop) (eqvs, prop) *)
  (*             in *)
  (*             UnderTy_base { basename; normalty; prop }) *)
  (*     | UnderTy_tuple ts -> UnderTy_tuple (List.map (aux ghost_t) ts) *)
  (*     | UnderTy_under_arrow { argty; retty } -> *)
  (*         UnderTy_under_arrow { argty; retty = aux ghost_t retty } *)
  (*     | UnderTy_over_arrow { argname; retty; argty } -> *)
  (*         UnderTy_over_arrow { argname; retty = aux ghost_t retty; argty } *)
  (*   in *)
  (*   aux None t *)

  let reduce_arrow_type_to_post t =
    let rec aux t =
      match t with
      | UnderTy_base _ -> t
      | UnderTy_tuple ts -> UnderTy_tuple (List.map aux ts)
      | UnderTy_under_arrow { retty; _ } -> aux retty
      | UnderTy_over_arrow { argname; retty; argty } ->
          let retty = aux retty in
          let { basename; normalty; prop } = argty in
          retty_add_ex_uprop_drop_independent
            (argname, UnderTy_base { basename; normalty; prop })
            retty
    in
    aux t
end

module MMT = struct
  module NT = Normalty.Ast.NT
  open Sexplib.Std

  type ut_with_copy =
    | UtNormal of T.t
    | UtCopy of string Normalty.Ast.Ntyped.typed
  [@@deriving sexp]

  type t = Ot of T.ot | Ut of ut_with_copy | Consumed of ut_with_copy
  [@@deriving sexp]

  open T

  let ut_eq_ = function
    | UtNormal ut1, UtNormal ut2 -> strict_eq ut1 ut2
    | UtCopy id1, UtCopy id2 -> Normalty.Ast.Ntyped.typed_eq id1 id2
    | _, _ -> false

  let eq_ = function
    | Ot ot1, Ot ot2 -> ot_strict_eq ot1 ot2
    | Ut ut1, Ut ut2 -> ut_eq_ (ut1, ut2)
    | Consumed ut1, Consumed ut2 -> ut_eq_ (ut1, ut2)
    | _, _ -> false

  let eq a b = eq_ (a, b)
  let ut_erase_ = function UtNormal ut -> erase ut | UtCopy id -> id.ty

  let erase = function
    | Ot ot -> ot.normalty
    | Ut ut -> ut_erase_ ut
    | Consumed ut -> ut_erase_ ut

  let ut_fv = function UtNormal ut -> fv ut | UtCopy id -> [ id.x ]

  let fv = function
    | Ot ot -> ot_fv ot
    | Consumed ut -> ut_fv ut
    | Ut ut -> ut_fv ut
end

module Utyped = struct
  include T

  type 'a typed = { x : 'a; ty : t } [@@deriving sexp]
end
