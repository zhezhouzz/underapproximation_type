let lemma1 =
  let l = (v : int list) true in
  let u = (v : int list) true in
  (v : int list) (implies (empty l) (not (mem l u)))

let lemma1 (u : 'forall * int) =
  let l = (v : int list) true in
  (v : int list) (implies (not (empty l)) (mem l u))
