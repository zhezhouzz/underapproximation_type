module T = struct
  open Sexplib.Std
  type t = string
  [@@deriving sexp]
end
