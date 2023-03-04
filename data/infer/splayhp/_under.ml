let partition =
  let pivot = (true : [%v: int]) in
  let tree =
    (fun (u : [%forall: int]) ->
       implies (mem v u) (hd v u) && implies (hd v u) (u <= pivot)
      : [%v: int splayhp])
  in
  ( left (fun (u : [%forall: int]) -> not (mem v u) : [%v: int splayhp]),
    right (fun (u : [%forall: int]) -> not (mem v u) : [%v: int splayhp]) )
