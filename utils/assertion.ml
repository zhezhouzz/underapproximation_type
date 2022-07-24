let _check_arity file line a b =
  if List.length a != List.length b then
    failwith (Printf.sprintf "Arity check error on %s line %i" file line)
  else ()

let _check_equality file line eq a b =
  if not @@ eq a b then
    failwith (Printf.sprintf "Equality check error on %s line %i" file line)
  else a

let _safe_combine file line a b =
  let () = _check_arity file line a b in
  List.combine a b

let _failatwith file line str =
  failwith (Printf.sprintf "[file %s line %i]: %s" file line str)

let _assert file line str b =
  if b then ()
  else
    failwith
      (Printf.sprintf "[file %s line %i]: Assertion fail with %s" file line str)
