type t = int list

val nil : t

val cons : int -> t -> t

val init : int -> t

val fold_left_list : (int -> t -> t) -> t -> t -> t

val fold_left_int : (int -> t -> int) -> int -> t -> int
