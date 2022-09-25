let[@library] nil = (fun (u : [%forall: int]) -> not (mem v u) : [%v: int list])

let[@library] cons =
  let l =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u) && not (empty v)
      : [%v: int list])
  in
  ( h (fun (u : [%forall: int]) -> iff (hd l u) (v == u) : [%v: int]),
    t (fun (u : [%forall: int]) -> implies (mem v u) (mem l u) : [%v: int list])
  )

let[@library] leaf =
  (fun (u : [%forall: int]) -> not (mem v u) : [%v: int tree])

let[@library] node =
  let tree =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u) && not (empty v)
      : [%v: int tree])
  in
  ( root (fun (u : [%forall: int]) -> iff (hd tree v) (v == u) : [%v: int]),
    left
      (fun (u : [%forall: int]) -> implies (mem v u) (hd tree u)
        : [%v: int tree]),
    right
      (fun (u : [%forall: int]) -> implies (mem v u) (hd tree u)
        : [%v: int tree]) )

(* leftisthp *)

let[@library] lhpleaf =
  (fun (u : [%forall: int]) -> not (mem v u) : [%v: int leftisthp])

let[@library] lhpnode =
  let tree =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u) && not (empty v)
      : [%v: int leftisthp])
  in
  ( r (true : [%v: int]),
    root (fun (u : [%forall: int]) -> iff (hd tree v) (v == u) : [%v: int]),
    left
      (fun (u : [%forall: int]) -> implies (mem v u) (hd tree u)
        : [%v: int leftisthp]),
    right
      (fun (u : [%forall: int]) -> implies (mem v u) (hd tree u)
        : [%v: int leftisthp]) )

(* pairinghp *)

let[@library] phpleaf =
  (fun (u : [%forall: int]) -> not (mem v u) : [%v: int pairinghp])

let[@library] phpnode =
  let tree =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u) && not (empty v)
      : [%v: int pairinghp])
  in
  ( root (fun (u : [%forall: int]) -> iff (hd tree v) (v == u) : [%v: int]),
    chd
      (fun (u : [%forall: int]) -> implies (mem v u) (hd tree u)
        : [%v: int pairinghp list]) )

(* binomialhp *)

let[@library] nil =
  (fun (u : [%forall: int]) -> not (mem v u) : [%v: int binomialhp list])

let[@library] cons =
  let l =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u)
      : [%v: int binomialhp list])
  in
  ( h
      (fun (u : [%forall: int]) -> implies (mem v u) (mem l u)
        : [%v: int binomialhp]),
    t
      (fun (u : [%forall: int]) -> implies (mem v u) (mem l u)
        : [%v: int binomialhp list]) )

(* rbset *)

let[@library] rbsleaf =
  (fun (u : [%forall: int]) -> not (mem v u) : [%v: int rbset])

let[@library] rbsnode =
  let tree =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u) && not (empty v)
      : [%v: int rbset])
  in
  ( color (true : [%v: bool]),
    left
      (fun (u : [%forall: int]) -> implies (mem v u) (hd tree u)
        : [%v: int rbset]),
    root (fun (u : [%forall: int]) -> iff (hd tree v) (v == u) : [%v: int]),
    right
      (fun (u : [%forall: int]) -> implies (mem v u) (hd tree u)
        : [%v: int rbset]) )

(* skewhp *)

let[@library] nil =
  (fun (u : [%forall: int]) -> not (mem v u) : [%v: int skewhp list])

let[@library] cons =
  let l =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u)
      : [%v: int skewhp list])
  in
  ( h
      (fun (u : [%forall: int]) -> implies (mem v u) (mem l u)
        : [%v: int skewhp]),
    t
      (fun (u : [%forall: int]) -> implies (mem v u) (mem l u)
        : [%v: int skewhp list]) )

(* splayhp *)

let[@library] sphpleaf =
  (fun (u : [%forall: int]) -> not (mem v u) : [%v: int splayhp])

let[@library] sphpnode =
  let tree =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u) && not (empty v)
      : [%v: int splayhp])
  in
  ( left
      (fun (u : [%forall: int]) -> implies (mem v u) (hd tree u)
        : [%v: int splayhp]),
    root (fun (u : [%forall: int]) -> iff (hd tree v) (v == u) : [%v: int]),
    right
      (fun (u : [%forall: int]) -> implies (mem v u) (hd tree u)
        : [%v: int splayhp]) )

(* unbset *)

let[@library] usleaf =
  (fun (u : [%forall: int]) -> not (mem v u) : [%v: int unbset])

let[@library] usnode =
  let tree =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u) && not (empty v)
      : [%v: int unbset])
  in
  ( left
      (fun (u : [%forall: int]) -> implies (mem v u) (hd tree u)
        : [%v: int unbset]),
    root (fun (u : [%forall: int]) -> iff (hd tree v) (v == u) : [%v: int]),
    right
      (fun (u : [%forall: int]) -> implies (mem v u) (hd tree u)
        : [%v: int unbset]) )
