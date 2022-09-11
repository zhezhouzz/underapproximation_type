let[@notation] nil = (v : int list) (fun (u : [%forall: int]) -> not (mem v u))

let[@notation] cons =
  let l =
    (v : int list) (fun (u : [%forall: int]) (w : [%forall: int]) ->
        implies (mem v u && mem v w) (u == w) && not (empty v))
  in
  ( (v : int) (mem l v),
    (v : int list) (fun (u : [%forall: int]) (w : [%forall: int]) ->
        implies (mem v u && mem v w) (u == w)) )

(* let[@notation] ileaf (u : [%forall: int]) = (v : int_tree) (not (mem v u)) *)

(* let[@notation] inode (u : [%forall: int]) (w : [%forall: int]) = *)
(*   let tree = *)
(*     (v : int_tree) (implies (mem v u && mem v w) (u == w) && not (empty v)) *)
(*   in *)
(*   ( (root : int) (mem tree root), *)
(*     (left : int_tree) (implies (mem left u && mem left w) (u == w)), *)
(*     (right : int_tree) (implies (mem right u && mem right w) (u == w)) ) *)
