open Lang
open Sugar

let make_order_constraint a x =
  let a = (AVar a) #: a.ty in
  let x = (AVar x) #: x.ty in
  let lt = "<" #: Nt.(construct_arr_tp ([ Ty_int; Ty_int ], Ty_bool)) in
  let geq = ">=" #: Nt.(construct_arr_tp ([ Ty_int; Ty_int ], Ty_bool)) in
  let ty = Nt._type_unify __FILE__ __LINE__ a.ty x.ty in
  match ty with
  | Nt.Ty_int ->
      And
        [
          Lit (AAppOp (lt, [ x; a ])) #: Nt.Ty_bool;
          Lit (AAppOp (geq, [ x; (AC (I 0)) #: Nt.Ty_int ])) #: Nt.Ty_bool;
        ]
  | _ -> _failatwith __FILE__ __LINE__ "unimp"

let typed_value_to_typed_lit file line v =
  match v.x with
  | VConst c -> (AC c) #: v.ty
  | VVar c -> (AVar c.x #: v.ty) #: v.ty
  | _ -> _failatwith file line "die"
