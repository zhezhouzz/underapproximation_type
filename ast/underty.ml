module T = struct
  module P = Autov.Prop
  module T = Autov.Smtty
  module NT = Normalty.Ast.NT
  module NTyped = Normalty.Ast.Ntyped
  open Sexplib.Std
  open Sugar

  type id = Strid.T.t [@@deriving sexp]
  type normalty = NT.t [@@deriving sexp]

  open NTyped

  (* invariant: the prop here should be existensial quantified. *)
  type t =
    | UnderTy_base of { basename : id; normalty : normalty; prop : P.t }
    | UnderTy_arrow of {
        argname : id;
        hidden_vars : id NTyped.typed list;
        argty : t;
        retty : t;
      }
    | UnderTy_tuple of t list
  [@@deriving sexp]

  open Zzdatatype.Datatype

  let hidden_vars_to_vars = List.map (fun x -> x.x)

  let var_space t =
    let add xs s = List.fold_left (fun s x -> StrMap.add x () s) s xs in
    let rec aux s = function
      | UnderTy_base { basename; prop; _ } ->
          let s = add (P.var_space prop) s in
          add [ basename ] s
      | UnderTy_tuple ts -> List.fold_left aux s ts
      | UnderTy_arrow { argname; hidden_vars; argty; retty } ->
          let s = add (argname :: hidden_vars_to_vars hidden_vars) s in
          let space =
            StrMap.to_key_list @@ aux (aux StrMap.empty argty) retty
          in
          add space s
    in
    StrMap.to_key_list @@ aux StrMap.empty t

  (* let rec destruct_arrow_tp = function *)
  (*   | UnderTy_arrow { argname; argty; retty } -> *)
  (*       let a, b = destruct_arrow_tp retty in *)
  (*       ((argty, argname) :: a, b) *)
  (*   | ty -> ([], ty) *)

  let rec erase = function
    | UnderTy_base { normalty; _ } -> normalty
    | UnderTy_arrow { argty; retty; _ } -> NT.Ty_arrow (erase argty, erase retty)
    | UnderTy_tuple ts -> NT.Ty_tuple (List.map erase ts)

  let subst_id t x y =
    let rec aux t =
      match t with
      | UnderTy_base { basename; normalty; prop } ->
          if String.equal basename x then t
          else UnderTy_base { basename; normalty; prop = P.subst_id prop x y }
      | UnderTy_arrow { argname; hidden_vars; argty; retty } ->
          let argty = aux argty in
          let retty =
            if
              List.exists (String.equal x)
                (argname :: hidden_vars_to_vars hidden_vars)
            then retty
            else aux retty
          in
          UnderTy_arrow { argname; hidden_vars; argty; retty }
      | UnderTy_tuple ts -> UnderTy_tuple (List.map aux ts)
    in
    aux t

  let assume_base_destruct_opt = function
    | UnderTy_base { basename; normalty; prop } ->
        Some (basename, normalty, prop)
    | _ -> None

  (* let mk_int_id name = P.{ ty = T.Int; x = name } *)

  let default_v_name = "v"

  let make_basic basename normalty prop =
    UnderTy_base
      { basename; normalty; prop = prop { x = basename; ty = normalty } }

  let make_basic_from_const_int (n : int) =
    make_basic default_v_name NT.Ty_int (fun nu ->
        P.(mk_lit_eq_lit (AVar nu) (ACint n)))

  let make_basic_from_const_bool (b : bool) =
    make_basic default_v_name NT.Ty_int
      P.(
        fun nu ->
          let nu = Lit (AVar nu) in
          match b with true -> nu | false -> Not nu)

  let make_basic_from_prop nt propf = make_basic default_v_name nt propf

  let make_basic_top normalty =
    make_basic default_v_name normalty (fun _ -> P.mk_true)

  let make_arrow_no_hidden_vars argname normalty argtyf rettyf =
    let id = { ty = normalty; x = argname } in
    UnderTy_arrow
      {
        argname;
        hidden_vars = [];
        argty = argtyf argname normalty;
        retty = rettyf id;
      }

  (* let arrow_args_rename args overftp = *)
  (*   let rec aux args overftp = *)
  (*     match (args, overftp) with *)
  (*     | [], tp -> tp *)
  (*     | id :: args, UnderTy_arrow { argname; argty; retty } -> *)
  (*         UnderTy_arrow *)
  (*           { *)
  (*             argname = id; *)
  (*             argty; *)
  (*             retty = aux args @@ subst_id retty argname id; *)
  (*           } *)
  (*     | _ -> _failatwith __FILE__ __LINE__ "" *)
  (*   in *)
  (*   aux args overftp *)

  let is_base_type = function UnderTy_base _ -> true | _ -> false

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
          List.for_all aux @@ _safe_combine __FILE__ __LINE__ ts1 ts2
      | ( UnderTy_arrow
            {
              argname = argname1;
              hidden_vars = hidden_vars1;
              argty = argty1;
              retty = retty1;
            },
          UnderTy_arrow
            {
              argname = argname2;
              hidden_vars = hidden_vars2;
              argty = argty2;
              retty = retty2;
            } ) ->
          String.equal argname1 argname2
          && List.equal NTyped.typed_eq hidden_vars1 hidden_vars2
          && aux (argty1, argty2)
          && aux (retty1, retty2)
      | _, _ -> false
    in
    aux (t1, t2)

  let eq a b = strict_eq a b

  let modify_prop_over_basetype f = function
    | ( UnderTy_base { basename = basename1; normalty = normalty1; prop = prop1 },
        UnderTy_base
          { basename = basename2; normalty = normalty2; prop = prop2 } ) ->
        let normalty =
          _check_equality __FILE__ __LINE__ NT.eq normalty1 normalty2
        in
        let prop2 =
          if String.equal basename1 basename2 then prop2
          else P.subst_id prop2 basename2 basename1
        in
        UnderTy_base { basename = basename1; normalty; prop = f prop1 prop2 }
    | _, _ -> _failatwith __FILE__ __LINE__ ""

  let modify_prop_in_ty f t1 t2 =
    let rec aux (t1, t2) =
      match (t1, t2) with
      | UnderTy_base _, UnderTy_base _ -> modify_prop_over_basetype f (t1, t2)
      | UnderTy_tuple ts1, UnderTy_tuple ts2 ->
          UnderTy_tuple (List.map aux @@ _safe_combine __FILE__ __LINE__ ts1 ts2)
      | ( UnderTy_arrow
            {
              argname = argname1;
              argty = argty1;
              hidden_vars = hidden_vars1;
              retty = retty1;
            },
          UnderTy_arrow
            {
              argname = argname2;
              argty = argty2;
              hidden_vars = hidden_vars2;
              retty = retty2;
            } ) ->
          (* NOTE: we ask the argument should be exactly the same *)
          let argname =
            _check_equality __FILE__ __LINE__ String.equal argname1 argname2
          in
          let argty =
            _check_equality __FILE__ __LINE__ strict_eq argty1 argty2
          in
          let hidden_vars =
            _check_equality __FILE__ __LINE__
              (List.equal NTyped.typed_eq)
              hidden_vars1 hidden_vars2
          in
          let retty = aux (retty1, retty2) in
          UnderTy_arrow { argname; hidden_vars; argty; retty }
      | _, _ -> _failatwith __FILE__ __LINE__ ""
    in
    aux (t1, t2)

  let disjunct = modify_prop_in_ty (fun x y -> P.Or [ x; y ])
  let conjunct = modify_prop_in_ty (fun x y -> P.And [ x; y ])

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

  let fv bodyt =
    let rec aux = function
      | UnderTy_base { basename; prop; _ } ->
          let fv = Autov.prop_fv prop in
          List.filter (fun x -> not @@ String.equal basename x) fv
      | UnderTy_tuple ts -> List.concat (List.map aux ts)
      | UnderTy_arrow { argname; hidden_vars; argty; retty } ->
          let fv_retty =
            List.filter
              (fun x ->
                not
                @@ List.exists (String.equal x)
                     (argname :: hidden_vars_to_vars hidden_vars))
              (aux retty)
          in
          let fv_argty =
            List.filter
              (fun x ->
                not
                @@ List.exists (String.equal x)
                     (hidden_vars_to_vars hidden_vars))
              (aux argty)
          in
          Zzdatatype.Datatype.List.slow_rm_dup String.equal (fv_retty @ fv_argty)
    in
    aux bodyt

  let is_fv_in name ty = List.exists (String.equal name) @@ fv ty

  let map_on_retty f t =
    let rec aux t =
      match t with
      | UnderTy_base { basename; normalty; prop } ->
          UnderTy_base { basename; normalty; prop = f prop }
      | UnderTy_tuple ts -> UnderTy_tuple (List.map aux ts)
      | UnderTy_arrow { argname; hidden_vars; argty; retty } ->
          UnderTy_arrow { argname; hidden_vars; argty; retty = aux retty }
    in
    aux t

  let eqv_to_bodyt { ty; x } =
    UnderTy_base { basename = x; normalty = ty; prop = P.mk_true }

  let join_tuple_t t = function
    | UnderTy_tuple ts -> UnderTy_tuple (ts @ [ t ])
    | _ -> _failatwith __FILE__ __LINE__ ""

  (* let add_ex_prop_qv_basic_raw (id, idprop) (basename, normalty, prop) = *)
  (*   UnderTy_base *)
  (*     { basename; normalty; prop = P.Exists (to_smttyped id, prop) } *)

  let work_on_retty if_apply (t_apply, f_apply) t =
    let rec aux t =
      match t with
      | UnderTy_base { basename; normalty; prop } ->
          if if_apply basename then
            UnderTy_base { basename; normalty; prop = t_apply prop }
          else UnderTy_base { basename; normalty; prop = f_apply prop }
      | UnderTy_tuple ts -> UnderTy_tuple (List.map aux ts)
      | UnderTy_arrow { argname; hidden_vars; argty; retty } ->
          let argty =
            if List.exists if_apply (hidden_vars_to_vars hidden_vars) then argty
            else _failatwith __FILE__ __LINE__ ""
          in
          let retty =
            if List.exists if_apply (argname :: hidden_vars_to_vars hidden_vars)
            then retty
            else aux retty
          in
          UnderTy_arrow { argname; hidden_vars; argty; retty }
    in
    aux t

  let add_ex_prop ifq id idty t =
    let id, idprop =
      match assume_base_destruct_opt idty with
      | None -> _failatwith __FILE__ __LINE__ "invalid idty"
      | Some (x', ty, prop) ->
          let prop = Autov.Prop.subst_id prop x' id in
          ({ x = id; ty }, prop)
    in
    let if_apply name = String.equal id.x name in
    let t_apply _ = _failatwith __FILE__ __LINE__ "" in
    let f_apply prop =
      if ifq then P.Exists (id, P.And [ idprop; prop ])
      else P.And [ idprop; prop ]
    in
    work_on_retty if_apply (t_apply, f_apply) t

  let add_ex_var id t =
    let if_apply name = String.equal id.x name in
    let t_apply prop = prop in
    let f_apply prop = P.Exists (id, prop) in
    work_on_retty if_apply (t_apply, f_apply) t

  let add_ex_vars ids t = List.fold_right add_ex_var ids t

  let instantiate_reduction vars t =
    let rec aux t =
      match t with
      | UnderTy_base { basename; normalty; prop } ->
          let prop = Autov.vars_reduction vars prop in
          UnderTy_base { basename; normalty; prop }
      | UnderTy_tuple ts -> UnderTy_tuple (List.map aux ts)
      | UnderTy_arrow { argname; hidden_vars; argty; retty } ->
          let argty = aux argty in
          let retty = aux retty in
          UnderTy_arrow { argname; hidden_vars; argty; retty }
    in
    aux t

  let instantiate_universial m t =
    let mk_conj prop m =
      P.And
        (List.map
           (fun l ->
             List.fold_left (fun prop (x, y) -> P.subst_id prop x y) prop l)
           m)
    in
    let filter m name =
      List.map (List.filter (fun (x, _) -> not @@ String.equal name x)) m
    in
    let filters m names = List.fold_left filter m names in
    let rec aux m t =
      match t with
      | UnderTy_base { basename; normalty; prop } ->
          let prop = mk_conj prop @@ filter m basename in
          UnderTy_base { basename; normalty; prop }
      | UnderTy_tuple ts -> UnderTy_tuple (List.map (aux m) ts)
      | UnderTy_arrow { argname; hidden_vars; argty; retty } ->
          let m = filters m @@ hidden_vars_to_vars @@ hidden_vars in
          let argty = aux m argty in
          let retty = aux (filter m argname) retty in
          UnderTy_arrow { argname; hidden_vars; argty; retty }
    in
    aux m t
end

module Utyped = struct
  include T

  type 'a typed = { x : 'a; ty : t } [@@deriving sexp]

  (* let map (f : 'a -> 'b) { x; ty } = { x = f x; ty } *)
  (* let typed_eq a b = String.equal a.x b.x && eq a.ty b.ty *)
end
