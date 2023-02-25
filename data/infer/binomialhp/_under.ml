let[@library] rank =
  let l1 =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u)
      : [%v: int binomialhp])
  in
  (true : [%v: int])

let[@library] link =
  let l1 =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u)
      : [%v: int binomialhp])
  in
  let l2 =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd l1 u)
      : [%v: int binomialhp])
  in
  (fun (u : [%forall: int]) -> implies (mem v u) (hd l1 u)
    : [%v: int binomialhp])

let[@library] ins_tree =
  let l1 =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u)
      : [%v: int binomialhp])
  in
  let l2 =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd l1 u)
      : [%v: int binomialhp list])
  in
  (fun (u : [%forall: int]) -> implies (mem v u) (hd l1 u)
    : [%v: int binomialhp list])

let merge =
  let ts1 =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u)
      : [%v: int binomialhp list])
  in
  let ts2 =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd ts1 u)
      : [%v: int binomialhp list])
  in
  (fun (u : [%forall: int]) -> implies (mem v u) (hd ts1 u) (* true *)
    : [%v: int binomialhp list])
