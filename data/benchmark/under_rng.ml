let[@library] eq =
  let (a : [%over: int]) = (true : [%v: int]) in
  let (b : [%over: int]) = (true : [%v: int]) in
  (iff v (a == b) : [%v: bool])

let[@library] neq =
  let (a : [%over: int]) = (true : [%v: int]) in
  let (b : [%over: int]) = (true : [%v: int]) in
  (iff v (a != b) : [%v: bool])

let[@library] lt =
  let (a : [%over: int]) = (true : [%v: int]) in
  let (b : [%over: int]) = (true : [%v: int]) in
  (iff v (a < b) : [%v: bool])

let[@library] gt =
  let (a : [%over: int]) = (true : [%v: int]) in
  let (b : [%over: int]) = (true : [%v: int]) in
  (iff v (a > b) : [%v: bool])

let[@library] le =
  let (a : [%over: int]) = (true : [%v: int]) in
  let (b : [%over: int]) = (true : [%v: int]) in
  (iff v (a <= b) : [%v: bool])

let[@library] ge =
  let (a : [%over: int]) = (true : [%v: int]) in
  let (b : [%over: int]) = (true : [%v: int]) in
  (iff v (a >= b) : [%v: bool])

let[@library] plus =
  let (a : [%over: int]) = (true : [%v: int]) in
  let (b : [%over: int]) = (true : [%v: int]) in
  (v == a + b : [%v: int])

let[@library] minus =
  let (a : [%over: int]) = (true : [%v: int]) in
  let (b : [%over: int]) = (true : [%v: int]) in
  (v == a - b : [%v: int])

let[@library] tt = (true : [%v: unit])
let[@library] nil = (rng v 0 : [%v: int list])

let[@library] cons =
  let (h : [%over: int]) = (true : [%v: int]) in
  let (s : [%over: int]) = (true : [%v: int]) in
  let (dummy : [%under: int list]) =
    (rng v s && fun (u : [%forall: int]) (w : [%forall: int]) ->
     implies (mem v u) (h <= u) && implies (ord v u w) (u <= w)
      : [%v: int list])
  in
  (fun (u : [%forall: int]) (w : [%forall: int]) ->
     implies (u == s + 1) (rng v u)
     && implies (mem v u) (h <= u)
     && implies (ord v u w) (u <= w)
    : [%v: int list])

let[@library] leaf = (rng v 0 : [%v: int tree])

let[@library] node =
  let (root : [%over: int]) = (true : [%v: int]) in
  let (range1 : [%over: int]) = (v >= 0 : [%v: int]) in
  let (dummy : [%under: int tree]) =
    (fun (u : [%forall: int]) ->
       implies (mem v u) (root - range1 < u && u < root)
       && sorted v && rng v range1
      : [%v: int tree])
  in
  let (ranger : [%over: int]) = (v >= 0 : [%v: int]) in
  let (dummy : [%under: int tree]) =
    (fun (u : [%forall: int]) ->
       implies (mem v u) (root < u && u < root + ranger)
       && sorted v && rng v ranger
      : [%v: int tree])
  in
  (fun (u : [%forall: int]) ->
     implies (u == range1 + ranger) (rng v u)
     && implies (mem v u) (root - range1 < u && u < root + ranger)
     && sorted v
    : [%v: int tree])

(* int set *)

let[@library] sempty = (rng v 0 : [%v: int set])

let[@library] snode =
  let (root : [%over: int]) = (true : [%v: int]) in
  let (range1 : [%over: int]) = (v >= 0 : [%v: int]) in
  let (dummy : [%under: int set]) =
    (fun (u : [%forall: int]) ->
       implies (mem v u) (root - range1 < u && u < root)
       && sorted v && rng v range1
      : [%v: int set])
  in
  let (ranger : [%over: int]) = (v >= 0 : [%v: int]) in
  let (dummy : [%under: int set]) =
    (fun (u : [%forall: int]) ->
       implies (mem v u) (root < u && u < root + ranger)
       && sorted v && rng v ranger
      : [%v: int set])
  in
  (fun (u : [%forall: int]) ->
     implies (u == range1 + ranger) (rng v u)
     && implies (mem v u) (root - range1 < u && u < root + ranger)
     && sorted v
    : [%v: int set])

(* unbset *)

let[@library] usleaf = (rng v 0 : [%v: int unbset])

let[@library] usnode =
  let (root : [%over: int]) = (true : [%v: int]) in
  let (range1 : [%over: int]) = (v >= 0 : [%v: int]) in
  let (dummy : [%under: int unbset]) =
    (fun (u : [%forall: int]) ->
       implies (mem v u) (root - range1 < u && u < root)
       && sorted v && rng v range1
      : [%v: int unbset])
  in
  let (ranger : [%over: int]) = (v >= 0 : [%v: int]) in
  let (dummy : [%under: int unbset]) =
    (fun (u : [%forall: int]) ->
       implies (mem v u) (root < u && u < root + ranger)
       && sorted v && rng v ranger
      : [%v: int unbset])
  in
  (fun (u : [%forall: int]) ->
     implies (u == range1 + ranger) (rng v u)
     && implies (mem v u) (root - range1 < u && u < root + ranger)
     && sorted v
    : [%v: int unbset])
