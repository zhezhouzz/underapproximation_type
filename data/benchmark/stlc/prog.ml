let nonderter_dec (a : int) : int =
  let (b : int) = a - 1 in
  int_range_inc 0 b

let nonderter_dec =
  let a = (v > 0 : [%v: int]) [@over] in
  (0 <= v && v < a : [%v: int]) [@under]

let rec type_eq (tau_a : stlc_ty) (tau_b : stlc_ty) : bool =
  match tau_a with
  | Stlc_ty_nat -> (
      match tau_b with
      | Stlc_ty_nat -> true
      | Stlc_ty_arr (tau_b_1, tau_b_2) -> false)
  | Stlc_ty_arr (tau_a_1, tau_a_2) -> (
      match tau_b with
      | Stlc_ty_nat -> false
      | Stlc_ty_arr (tau_b_1, tau_b_2) ->
          if type_eq tau_a_1 tau_b_1 then type_eq tau_a_2 tau_b_2 else false)

let type_eq =
  let tau_a = (true : [%v: stlc_ty]) [@over] in
  let tau_b = (true : [%v: stlc_ty]) [@over] in
  (iff v (type_eq_spec tau_a tau_b) : [%v: bool]) [@under]
