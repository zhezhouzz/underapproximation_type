let ctx = Z3.mk_context [("model", "true"); ("proof", "false"); ("timeout", "1999")]

let check vc = Check.check ctx vc
