open Sexplib.Std
open Mtyped

type 't ctx = Typectx of ('t, string) typed list [@@deriving sexp]

let rec map_ctx (f : 't -> 's) (ctx_e : 't ctx) =
  match ctx_e with
  | Typectx _t_stringtypedlist0 ->
      Typectx (List.map (fun x -> x #=> f) _t_stringtypedlist0)

and typed_map_ctx (f : 't -> 's) (ctx_e : ('t, 't ctx) typed) =
  ctx_e #-> (map_ctx f)

(* Generated from _typectx.ml *)
