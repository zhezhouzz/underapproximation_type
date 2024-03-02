open Languagez
open Sugar

type t = Nt.t

let bi_typed_id_check (ctx : t ctx) (x : (t option, string) typed) (ty : t) :
    (t, string) typed =
  let ty' =
    match get_opt ctx x.x with
    | None -> _failatwith __FILE__ __LINE__ "die"
    | Some ty -> ty
  in
  match x.ty with
  | None -> { ty = ty'; x = x.x }
  | Some ty' ->
      let ty = Nt._type_unify __FILE__ __LINE__ ty' ty in
      { ty; x = x.x }

let bi_typed_id_infer (ctx : t ctx) (x : (t option, string) typed) :
    (t, string) typed =
  let ty =
    match get_opt ctx x.x with
    | None -> _failatwith __FILE__ __LINE__ "die"
    | Some ty -> ty
  in
  { ty; x = x.x }
