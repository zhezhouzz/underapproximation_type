open Language
open Normal_prop_typing

type t = Nt.t

let bi_typed_cty_check (ctx : t ctx) (cty : t option cty) : t cty =
  match cty with
  | Cty { phi; nty } ->
      let v = default_v #: nty in
      Cty { phi = bi_typed_prop_check (add_to_right ctx v) phi; nty }
