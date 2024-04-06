open Syntax

(* open Typedlang *)
open Propfunc
open Zzdatatype.Datatype
open Sugar

type t = Nt.t

let prop_to_cty nty prop = Cty { nty; phi = prop }

let mk_cty_var_eq_c nty (id, c) =
  Cty { nty; phi = mk_prop_var_eq_c nty (id, c) }

let mk_cty_var_eq_var nty (id, c) =
  Cty { nty; phi = mk_prop_var_eq_var nty (id, c) }

let n_to_one_ctys prop_f = function
  | [] -> _failatwith __FILE__ __LINE__ "die"
  | Cty { nty; phi } :: ctys ->
      if List.for_all (function Cty { nty = nty'; _ } -> Nt.eq nty nty') ctys
      then
        let phi =
          prop_f (phi :: List.map (function Cty { phi; _ } -> phi) ctys)
        in
        Cty { nty; phi }
      else _failatwith __FILE__ __LINE__ "die"

let union_ctys = n_to_one_ctys smart_or
let intersect_ctys = n_to_one_ctys smart_and

let forall_cty_to_prop = function
  | { x; ty = Cty { nty; phi } }, prop ->
      let x = x #: nty in
      let phi_x = subst_prop_instance default_v (AVar x) phi in
      smart_pi (x, phi_x) prop

let exists_cty_to_prop = function
  | { x; ty = Cty { nty; phi } }, prop ->
      let x = x #: nty in
      let phi_x = subst_prop_instance default_v (AVar x) phi in
      smart_sigma (x, phi_x) prop

let forall_cty_to_cty = function
  | x, Cty { nty; phi } -> Cty { nty; phi = forall_cty_to_prop (x, phi) }

let exists_cty_to_cty = function
  | x, Cty { nty; phi } -> Cty { nty; phi = exists_cty_to_prop (x, phi) }

let map_phi_in_cty f = function Cty { nty; phi } -> Cty { nty; phi = f phi }
let and_prop_to_cty (phi_x, cty) = map_phi_in_cty (smart_add_to phi_x) cty
let and_cty_to_cty = function Cty { phi; _ }, cty -> and_prop_to_cty (phi, cty)
