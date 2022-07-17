let rec f x l = match x :: l with [] -> x | h :: t -> f h t
