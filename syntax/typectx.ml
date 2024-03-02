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
open Sugar

let get_opt ctx name =
  match ctx with
  | Typectx l ->
      let* x = List.find_opt (fun x -> String.equal name x.x) l in
      Some x.ty

let add_to_right ctx { x; ty } =
  match get_opt ctx x with
  | Some _ -> _failatwith __FILE__ __LINE__ "duplicate adding to ctx"
  | None -> ( match ctx with Typectx l -> Typectx (l @ [ { x; ty } ]))

let add_to_rights ctx l = List.fold_left add_to_right ctx l
