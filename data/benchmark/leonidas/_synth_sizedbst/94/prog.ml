let rec goal (d : int) (s0 : int) (lo : int) (hi : int) =
  (if lt_eq_one d
   then
     Node (hi, (goal d1 d1 hi hi), (goal d1 (increment d) (increment s0) hi))
   else
     if bool_gen ()
     then goal (increment d) (increment s) d1 (increment d1)
     else
       Node
         ((increment s), (goal (increment lo) d hi hi),
           (goal (increment lo) d hi hi)) : int tree)
