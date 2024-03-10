open Language
open Normal_cty_typing

type t = Nt.t

let bi_typed_rty_check (ctx : t ctx) (rty : t option rty) : t rty =
  let rec aux ctx = function
    | RtyBase { ou; cty } -> RtyBase { ou; cty = bi_typed_cty_check ctx cty }
    | RtyBaseArr { argcty; arg; retty } ->
        let argcty = bi_typed_cty_check ctx argcty in
        let arg' = arg #: (erase_cty argcty) in
        RtyBaseArr { argcty; arg; retty = aux (add_to_right ctx arg') retty }
    | RtyArrArr { argrty; retty } ->
        let argrty = aux ctx argrty in
        RtyArrArr { argrty; retty = aux ctx retty }
    | RtyTuple _trtylist0 -> RtyTuple (List.map (aux ctx) _trtylist0)
  in
  aux ctx rty
