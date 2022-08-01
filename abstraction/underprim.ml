open Zzdatatype.Datatype
module NT = Languages.Normalty
module P = Autov.Prop
module T = Autov.Smtty

let tab_names =
  [
    ("intlt", "<");
    ("intgt", ">");
    ("intle", "<=");
    ("intge", ">=");
    ("inteq", "==");
    ("intlistnil", "[]");
    ("intlistcons", "::");
    ("intadd", "+");
    ("intsub", "-");
  ]

let rev_tab_names = [ ("rev_intlistnil", "[]"); ("rev_intlistcons", "::") ]
let m = ref None
let rev_m = ref None

let make_m (refinements : (string * Languages.Underty.t) list) =
  let tab, rev_tab, rest =
    List.fold_left
      (fun (m, rev_m, rest) (x, ty) ->
        match List.find_opt (fun (idx, _) -> String.equal idx x) tab_names with
        | Some (_, name) -> ((name, ty) :: m, rev_m, rest)
        | None -> (
            match
              List.find_opt (fun (idx, _) -> String.equal idx x) rev_tab_names
            with
            | Some (_, name) -> (m, (name, ty) :: rev_m, rest)
            | None -> (m, rev_m, (x, ty) :: rest)))
      ([], [], []) refinements
  in
  let check tab m =
    List.iter
      (fun (_, name) ->
        match List.find_opt (fun (x, _) -> String.equal name x) m with
        | None -> failwith "cannot find under type of built-in prim"
        | Some _ -> ())
      tab
  in
  let () = check tab_names tab in
  let () = check rev_tab_names rev_tab in
  m := Some (StrMap.from_kv_list @@ tab @ rest);
  rev_m := Some (StrMap.from_kv_list rev_tab)
