val empty : unit -> elem list
val insert : elem -> elem list -> elem list

let[@library] empty =
  let (i [@ghost]) = (0 == v : [%v: int]) [@over] in
  let u = (true : [%v: unit]) [@over] in
  (sorted v && len v i : [%v: elem list]) [@under]

let[@library] insert =
  let (i [@ghost]) = (0 < v : [%v: int]) [@over] in
  let x = (true : [%v: elem]) [@under] in
  let s = (sorted v && len v (i - 1) : [%v: elem list]) [@under] in
  (sorted v && len v i : [%v: elem list]) [@under]

let rec inv (u : unit) : elem list =
  if bool_gen () then empty ()
  else
    let (s : elem list) = inv () in
    insert (elem_gen ()) s

let[@assert] inv =
  let (i [@ghost]) = (0 <= v : [%v: int]) [@over] in
  let u = (true : [%v: unit]) [@over] in
  (sorted v && len v i : [%v: elem list]) [@under]
