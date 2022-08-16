module F (Type : Type.T) = struct
  open Sexplib.Std

  type t = (string * Type.t) list [@@deriving sexp]

  let add_to_right ctx (ty, x) = ctx @ [ (x, ty) ]
  let get_opt ctx id = List.find_opt (fun (y, _) -> String.equal y id) ctx
  let empty = []
end

module NSimpleTypectx = F (Normalty.T)
module SMTSimpleTypectx = F (Autov.Smtty)
