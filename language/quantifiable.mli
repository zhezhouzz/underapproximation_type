module type T = sig
  type t [@@deriving sexp]

  val eq : t -> t -> bool
  val var_space : t -> string list
  val fv : t -> string list
  val subst_id : t -> string -> string -> t
end
