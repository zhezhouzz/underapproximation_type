module T : Ast.Id = struct
  open Sexplib.Std
  type t = string
  [@@deriving sexp]
end
