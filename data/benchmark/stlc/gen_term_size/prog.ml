let rec gen_term_size (dec : int) (num_app : int) (tau : stlc_ty)
    (gamma : stlc_tyctx) : stlc_term =
  if num_app == 0 then
    let (n1 : int) = nat_gen () in
    gen_term_no_app n1 tau gamma
  else
    let (dec_dec : int) = nonderter_dec dec in
    let (num_app1 : int) = num_app - 1 in
    let (num_app_func : int) = int_range_inc 0 num_app1 in
    let (num_app_arg : int) = int_range_inc 0 (num_app1 - num_app_func) in
    let (arg_tau : stlc_ty) = gen_type_size (nat_gen ()) in
    let (func : stlc_term) =
      gen_term_size dec_dec num_app_func (Stlc_ty_arr (arg_tau, tau)) gamma
    in
    let (arg : stlc_term) = gen_term_size dec_dec num_app_arg arg_tau gamma in
    let (app_term : stlc_term) = Stlc_app (func, arg) in
    let (close_term : stlc_term) =
      match tau with
      | Stlc_ty_nat -> gen_const ()
      | Stlc_ty_arr (tau1, tau2) ->
          let (body : stlc_term) =
            gen_term_size dec_dec num_app tau2 (Stlc_tyctx_cons (tau1, gamma))
          in
          Stlc_abs (tau1, body)
    in
    if bool_gen () then or_var_in_typectx close_term tau gamma
    else or_var_in_typectx app_term tau gamma
