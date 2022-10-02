let[@library] eq =
  let (a : [%poly: int]) = () in
  let (b : [%poly: int]) = () in
  (iff v (a == b) : [%v: bool])

let[@library] neq =
  let (a : [%poly: int]) = () in
  let (b : [%poly: int]) = () in
  (iff v (a != b) : [%v: bool])

let[@library] lt =
  let (a : [%poly: int]) = () in
  let (b : [%poly: int]) = () in
  (iff v (a < b) : [%v: bool])

let[@library] gt =
  let (a : [%poly: int]) = () in
  let (b : [%poly: int]) = () in
  (iff v (a > b) : [%v: bool])

let[@library] le =
  let (a : [%poly: int]) = () in
  let (b : [%poly: int]) = () in
  (iff v (a <= b) : [%v: bool])

let[@library] ge =
  let (a : [%poly: int]) = () in
  let (b : [%poly: int]) = () in
  (iff v (a => b) : [%v: bool])

let[@library] plus =
  let (a : [%poly: int]) = () in
  let (b : [%poly: int]) = () in
  (v == a + b : [%v: int])

let[@library] minus =
  let (a : [%poly: int]) = () in
  let (b : [%poly: int]) = () in
  (v == a - b : [%v: int])

let[@library] nil = (empty v : [%v: int list])

let[@library] cons =
  let (h : [%poly: int]) = () in
  let t = (fun (u : [%forall: int]) -> not (ord v u u) : [%v: int list]) in
  (fun (u : [%forall: int]) ->
     hd v h && implies (not (u == h)) (not (ord v u u))
    : [%v: int list])

let[@library] leaf =
  (fun (u : [%forall: int]) -> not (mem v u) : [%v: int tree])

let[@library] node =
  let (root : [%poly: int]) = () in
  let left =
    (fun (u : [%forall: int]) ->
       implies (hd v u) (u == root) && implies (mem v u) (u == root)
      : [%v: int tree])
  in
  let right =
    (fun (u : [%forall: int]) ->
       implies (hd v u) (u == root) && implies (mem v u) (u == root)
      : [%v: int tree])
  in
  (fun (u : [%forall: int]) ->
     iff (hd v u) (u == root) && implies (mem v u) (u == root)
    : [%v: int tree])

(* leftisthp *)

let[@library] lhpleaf =
  (fun (u : [%forall: int]) -> not (mem v u) : [%v: int leftisthp])

let[@library] lhpnode =
  let (r : [%poly: int]) = () in
  let (root : [%poly: int]) = () in
  let left =
    (fun (u : [%forall: int]) ->
       implies (hd v u) (u == root) && implies (mem v u) (u == root)
      : [%v: int leftisthp])
  in
  let right =
    (fun (u : [%forall: int]) ->
       implies (hd v u) (u == root) && implies (mem v u) (u == root)
      : [%v: int leftisthp])
  in
  (fun (u : [%forall: int]) ->
     iff (hd v u) (u == root) && implies (mem v u) (u == root)
    : [%v: int leftisthp])

(* pairinghp *)

let[@library] phpleaf =
  (fun (u : [%forall: int]) -> not (mem v u) : [%v: int pairinghp])

let[@library] phpnode =
  let (root : [%poly: int]) = () in
  let chd =
    (fun (u : [%forall: int]) ->
       implies (hd v u) (u == root) && implies (mem v u) (u == root)
      : [%v: int pairinghp list])
  in
  (fun (u : [%forall: int]) ->
     iff (hd v u) (u == root) && implies (mem v u) (u == root)
    : [%v: int pairinghp])

let[@library] nil =
  (fun (u : [%forall: int]) -> not (mem v u) : [%v: int pairinghp list])

let[@library] cons =
  let h =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u)
      : [%v: int pairinghp])
  in
  let t =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u)
      : [%v: int pairinghp list])
  in
  (fun (u : [%forall: int]) -> implies (mem v u) (hd v u) && not (empty v)
    : [%v: int pairinghp list])

(* binomialhp *)

let[@library] nil =
  (fun (u : [%forall: int]) -> not (mem v u) : [%v: int binomialhp list])

let[@library] cons =
  let h =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u)
      : [%v: int binomialhp])
  in
  let t =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u)
      : [%v: int binomialhp list])
  in
  (fun (u : [%forall: int]) -> implies (mem v u) (hd v u) && not (empty v)
    : [%v: int binomialhp list])

(* rbset *)

let[@library] rbsleaf =
  (fun (u : [%forall: int]) -> not (mem v u) : [%v: int rbset])

let[@library] rbsnode =
  let (color : [%poly: bool]) = () in
  let left =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u) : [%v: int rbset])
  in
  let root =
    (fun (u : [%forall: int]) -> implies (hd left u) (u == v) : [%v: int])
  in
  let right =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd left u)
      : [%v: int rbset])
  in
  (fun (u : [%forall: int]) ->
     iff (hd v u) (u == root) && implies (mem v u) (hd left u)
    : [%v: int rbset])

(* skewhp *)

let[@library] nil =
  (fun (u : [%forall: int]) -> not (mem v u) : [%v: int skewhp list])

let[@library] cons =
  let h =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u) : [%v: int skewhp])
  in
  let t =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u)
      : [%v: int skewhp list])
  in
  (fun (u : [%forall: int]) -> implies (mem v u) (hd v u) && not (empty v)
    : [%v: int skewhp list])

(* splayhp *)

let[@library] sphpleaf =
  (fun (u : [%forall: int]) -> not (mem v u) : [%v: int splayhp])

let[@library] sphpnode =
  let left =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u) : [%v: int splayhp])
  in
  let root =
    (fun (u : [%forall: int]) -> implies (hd left u) (u == v) : [%v: int])
  in
  let right =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd left u)
      : [%v: int splayhp])
  in
  (fun (u : [%forall: int]) ->
     iff (hd v u) (u == root) && implies (mem v u) (hd left u)
    : [%v: int splayhp])

(* unbset *)

let[@library] usleaf =
  (fun (u : [%forall: int]) -> not (mem v u) : [%v: int unbset])

let[@library] usnode =
  let left =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u) : [%v: int unbset])
  in
  let root =
    (fun (u : [%forall: int]) -> implies (hd left u) (u == v) : [%v: int])
  in
  let right =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd left u)
      : [%v: int unbset])
  in
  (fun (u : [%forall: int]) ->
     iff (hd v u) (u == root) && implies (mem v u) (hd left u)
    : [%v: int unbset])
