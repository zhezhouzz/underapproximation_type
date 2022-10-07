open Languages

(* TODO: a real persistence check *)
let persistence_check _ ty = if UT.is_base_type ty then false else true
