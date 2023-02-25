let rec merge_trees (ts1 : int skewhp list) (ts2 : int skewhp list) :
    int skewhp list =
  match ts1 with
  | [] -> ts2
  | t1 :: ts11 -> (
      match ts2 with
      | [] -> ts1
      | t2 :: ts21 ->
          let (r1 : int) = rank t1 in
          let (r2 : int) = rank t2 in
          let (b1 : bool) = r1 < r2 in
          if b1 then
            let (tmp0 : int skewhp list) = merge_trees ts11 ts2 in
            let (tmp1 : int skewhp list) = t1 :: tmp0 in
            tmp1
          else
            let (b2 : bool) = r2 < r1 in
            if b2 then
              let (tmp2 : int skewhp list) = merge_trees ts1 ts21 in
              let (tmp3 : int skewhp list) = t2 :: tmp2 in
              tmp3
            else
              let (tmp4 : int skewhp list) = merge_trees ts11 ts21 in
              let (tmp5 : int skewhp) = link t1 t2 in
              let (tmp6 : int skewhp list) = ins_tree tmp5 tmp4 in
              tmp6)
