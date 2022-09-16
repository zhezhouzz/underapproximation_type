module type T = sig
  include Type.T

  val fv : t -> string list
  val var_space : t -> string list
  val subst_id : t -> string -> string -> t
  val conjunct : t -> t -> t
  val conjunct_list : t list -> t
end
