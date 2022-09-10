open Languages
module Typectx = UnderTypectx
module P = Autov.Prop
open Ntyped

(* open Zzdatatype.Datatype *)
(* open Abstraction *)
(* open Sugar *)

type mp_maps = (string * string typed) list

let get_bv model m =
  let m_z3 =
    List.map
      (fun (mp, args) ->
        Autov.get_mp_app model (mp, List.map (fun x -> x.x) args))
      m
  in
  m_z3

let bv_to_prop m l =
  let open P in
  let prop =
    Not
      (And
         (List.map (fun ((mp, args), b) ->
              let prop = MethodPred (mp, List.map (fun x -> AVar x) args) in
              if b then prop else Not prop)
         @@ List.combine m l))
  in
  prop

let default_m =
  let open Ntyped in
  [
    ("mem", [ { ty = Ty_int; x = "v" }; { ty = Ty_int; x = "u" } ]);
    ("mem", [ { ty = Ty_int; x = "s1" }; { ty = Ty_int; x = "u" } ]);
    ("mem", [ { ty = Ty_int; x = "s2" }; { ty = Ty_int; x = "u" } ]);
  ]

open UL

let rec_infer ctx (f : string UL.typed) (body : NL.value NL.typed) typecheck :
    value typed =
  let m = default_m in
  let rec loop props =
    let _ = Pp.printf "@{<bold>Iter %i:@}\n" (List.length props) in
    let f =
      { x = f.x; ty = UT.map_on_retty (fun x -> P.And (props @ [ x ])) f.ty }
    in
    let ctx' = Typectx.add_to_right ctx f in
    try
      let body = typecheck ctx' body f.ty in
      { ty = body.ty; x = Fix (f, body) }
    with Autov.FailWithModel (msg, model) ->
      let () = Pp.printf "@{<orange>Application failed:@}%s\n" msg in
      let bv' = get_bv model m in
      let prop = bv_to_prop m bv' in
      loop (prop :: props)
  in
  loop []
