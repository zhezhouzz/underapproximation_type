type 'a t

val empty : 'a t
val exists : 'a t -> string -> bool
val get_opt : 'a t -> string -> 'a option
val get_ty : 'a t -> string -> 'a
val add_to_left : 'a * string -> 'a t -> 'a t
val add_to_right : 'a t -> 'a * string -> 'a t
val add_to_rights : 'a t -> ('a * string) list -> 'a t
val pretty_layout : ('a -> string) -> 'a t -> string
val subtract : 'a t -> 'a t -> 'a t
val fold_right : (string * 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val filter_map : (string * 'a -> (string * 'b) option) -> 'a t -> 'b t
val fv : ('a -> string list) -> 'a t -> string list
val update : 'a t -> string * ('a -> 'a) -> 'a t
