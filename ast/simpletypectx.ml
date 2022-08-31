module F (Type : Type.T) = struct
  open Sexplib.Std

  type t = (string * Type.t) list [@@deriving sexp]

  let add_to_right ctx (ty, x) = ctx @ [ (x, ty) ]
  let get_opt ctx id = List.find_opt (fun (y, _) -> String.equal y id) ctx
  let empty = []
end

module NSimpleTypectx = struct
  include F (Normalty.T)

  let of_type_decls e : t = Type_dec.T.mk_ctx e
end

module SMTSimpleTypectx = F (Autov.Smtty)
module UTSimpleTypectx = F (Underty.T)
