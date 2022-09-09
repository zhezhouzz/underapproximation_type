open Prop.T

type mp_maps = (string * lit list) list

open Zzdatatype.Datatype

let init m = BoolVec.init_as_min @@ List.length m

let bv_to_prop m bv =
  let l = BoolVec.to_list bv in
  let prop =
    Not
      (And
         (List.map (fun ((mp, args), b) ->
              let prop = MethodPred (mp, args) in
              if b then prop else Not prop)
         @@ List.combine m l))
  in
  prop

let next bv = BoolVec.increase bv

let fold_left (f : 'a -> t -> 'a option) default m =
  let bv = init m in
  let rec loop x bv =
    let prop = bv_to_prop m bv in
    match f x prop with
    | None -> x
    | Some x' -> (
        match next bv with None -> failwith "end" | Some bv' -> loop x' bv')
  in
  loop default bv

open Normalty.Ast.T

let default_m =
  [
    ("hd", [ AVar { ty = Ty_int; x = "v" }; AVar { ty = Ty_int; x = "u" } ]);
    ("hd", [ AVar { ty = Ty_int; x = "l1" }; AVar { ty = Ty_int; x = "u" } ]);
    ("hd", [ AVar { ty = Ty_int; x = "l2" }; AVar { ty = Ty_int; x = "u" } ]);
  ]
