open Zzdatatype.Datatype
module NT = Languages.Normalty
module P = Autov.Prop
module T = Autov.Smtty

let tab_names = [ ("lt", "<"); ("intlistnil", "[]"); ("intlistcons", "::") ]
let rev_tab_names = [ ("rev_intlistnil", "[]"); ("rev_intlistcons", "::") ]
let m = ref None
let rev_m = ref None

let make_m (refinements : (string * Languages.Underty.t) list) =
  let get_tab tab =
    List.map
      (fun (idx, name) ->
        match List.find_opt (fun (x, _) -> String.equal idx x) refinements with
        | None -> failwith "die: get under prim tab"
        | Some (_, ty) -> (name, ty))
      tab
  in
  m := Some (StrMap.from_kv_list @@ get_tab tab_names);
  rev_m := Some (StrMap.from_kv_list @@ get_tab rev_tab_names)
