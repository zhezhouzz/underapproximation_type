type t = int * List.t

val create : int -> int -> t ref

val copy : t ref -> t ref -> unit

val merge_list : t ref -> t ref -> unit
