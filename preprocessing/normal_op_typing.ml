open Languagez
open Normal_id_typing

type t = Nt.t

let bi_typed_op_check (ctx : t ctx) (op : (t option, op) typed) (ty : t) :
    (t, op) typed =
  match op.x with
  | PrimOp id ->
      let id = bi_typed_id_check ctx id #: op.ty ty in
      (PrimOp id.x) #: id.ty
  | DtConstructor id ->
      let id = bi_typed_id_check ctx id #: op.ty ty in
      (DtConstructor id.x) #: id.ty

let bi_typed_op_infer (ctx : t ctx) (op : (t option, op) typed) : (t, op) typed
    =
  match op.x with
  | PrimOp id ->
      let id = bi_typed_id_infer ctx id #: op.ty in
      (PrimOp id.x) #: id.ty
  | DtConstructor id ->
      let id = bi_typed_id_infer ctx id #: op.ty in
      (DtConstructor id.x) #: id.ty
