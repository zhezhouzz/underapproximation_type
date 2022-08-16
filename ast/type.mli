module type T = sig
  type t [@@deriving sexp]

  val eq : t -> t -> bool
  (* val destruct_arrow_tp : t -> t list * t *)
  (* val construct_arrow_tp : t list * t -> t *)
end
