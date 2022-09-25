let[@library] rank =
  let l1 =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u) : [%v: int skewhp])
  in
  (true : [%v: int])

let[@library] link =
  let l1 =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u) : [%v: int skewhp])
  in
  let l2 =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd l1 u) : [%v: int skewhp])
  in
  (fun (u : [%forall: int]) -> implies (mem v u) (hd l1 u) : [%v: int skewhp])

let[@library] ins_tree =
  let l1 =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u) : [%v: int skewhp])
  in
  let l2 =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd l1 u)
      : [%v: int skewhp list])
  in
  (fun (u : [%forall: int]) -> implies (mem v u) (hd l1 u)
    : [%v: int skewhp list])

let merge_trees =
  let ts1 =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd v u)
      : [%v: int skewhp list])
  in
  let ts2 =
    (fun (u : [%forall: int]) -> implies (mem v u) (hd ts1 u)
      : [%v: int skewhp list])
  in
  (fun (u : [%forall: int]) ->
     (* true *)
     implies (mem v u) (hd ts1 u)
    : [%v: int skewhp list])
