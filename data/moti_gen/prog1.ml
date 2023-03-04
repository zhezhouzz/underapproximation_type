let merge (l1 : int list) (l2 : int list) : int list =
  match l1 with
  | [] -> Exn (* l2 *)
  | h1 :: t1 -> (
      match l2 with
      | [] -> Exn (* l1 *)
      | h2 :: t2 ->
          let (b1 : bool) = h1 < h2 in
          if b1 then
            (* let (tmp0 : int list) = merge0 h1 h2 t1 l2 in *)
            (* let (tmp1 : int list) = h1 :: tmp0 in *)
            (* tmp1 *)
            Exn
          else
            let (b2 : bool) = h2 < h1 in
            if b2 then
              (* h1 = hd(l1), sort(l1) /\ not empty(l1), sort(t2) *)
              let (tmp2 : int list) = merge0 t1 l2 in
              (* h1 = hd(l1), sort(l1) /\ not empty(l1), sort(t2), sort(tmp2) /\ not empty(l1) *)
              let (tmp3 : int list) = h1 :: tmp2 in
              (* h1 = hd(l1), sort(l1) /\ not empty(l1), sort(t2), sort(tmp2), sort(tmp2) besides head /\ not empty(l1) *)
              tmp3
            else
              (* let (tmp4 : int list) = merge0 h1 h2 t1 t2 in *)
              (* let (tmp5 : int list) = h1 :: tmp4 in *)
              (* tmp5 *)
              Exn)
