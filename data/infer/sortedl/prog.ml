let rec merge (l1 : int list) (l2 : int list) : int list =
  match l1 with
  | [] -> l2
  | hd1 :: tl1 -> (
      match l2 with
      | [] -> l1
      | hd2 :: tl2 ->
          let (b1 : bool) = hd1 < hd2 in
          let (tmp0 : int list) = merge tl1 tl2 in
          if b1 then
            let (tmp1 : int list) = hd2 :: tmp0 in
            let (tmp2 : int list) = hd1 :: tmp1 in
            tmp2
          else
            let (b2 : bool) = hd2 < hd1 in
            if b2 then
              let (tmp3 : int list) = hd1 :: tmp0 in
              let (tmp4 : int list) = hd2 :: tmp3 in
              tmp4
            else
              let (tmp5 : int list) = hd1 :: tmp0 in
              tmp5)
