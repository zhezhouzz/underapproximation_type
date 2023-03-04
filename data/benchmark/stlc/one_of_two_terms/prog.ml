let one_of_two_terms (a : stlc_term) (b : stlc_term) : stlc_term =
  if bool_gen () then a else b
