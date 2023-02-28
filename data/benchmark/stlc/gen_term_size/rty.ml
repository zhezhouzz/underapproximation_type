let gen_term_size =
  let dec = (v >= 0 : [%v: int]) [@over] in
  let num_app = (v >= 0 : [%v: int]) [@over] in
  let tau = (dec_pair dec num_app v : [%v: stlc_ty]) [@over] in
  let gamma = (true : [%v: stlc_tyctx]) [@over] in
  (typing gamma v tau && size_app v num_app : [%v: stlc_term]) [@under]
