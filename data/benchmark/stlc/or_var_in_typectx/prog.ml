let or_var_in_typectx (a : stlc_term) (tau : stlc_ty) (gamma : stlc_tyctx) :
    stlc_term =
  if bool_gen () then a else vars_with_type gamma tau
