let foo (l : int list) : int list = match l with [] -> Exn | h :: t -> t
