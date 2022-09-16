module type T = sig
  include Quantifiable.T

  val conjunct : t -> t -> t
  val conjunct_list : t list -> t
end
