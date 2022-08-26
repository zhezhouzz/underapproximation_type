let rec merge (l1 : int list) (l2 : int list) =
  match l1 with
  | [] -> l2
  | hd1 :: tl1 -> (
      match l2 with
      | [] -> l1
      | hd2 :: tl2 ->
          if hd1 < hd2 then hd1 :: hd2 :: merge tl1 tl2
          else if hd2 < hd1 then hd2 :: hd1 :: merge tl1 tl2
          else hd1 :: merge tl1 tl2)
