open Zzdatatype.Datatype
module NT = Languages.Normalty
module P = Autov.Prop
module T = Autov.Smtty

let tab_names = [ ("lt", "<"); ("intlistnil", "[]"); ("intlistcons", "::") ]
let rev_tab_names = [ ("rev_intlistnil", "[]"); ("rev_intlistcons", "::") ]
let m = ref None
let rev_m = ref None

let make_m under_path =
  let open Frontend in
  let x =
    Ocaml_parser.Frontend.parse ~sourcefile:under_path
    (* ^ "/Users/zhezhou/workspace/research/underapproximation_type/data/prim/under.ml" *)
  in
  let refinements =
    Structure.refinement_of_ocamlstruct Undertype.undertype_of_ocamlexpr x
  in
  (* let () = *)
  (*   Printf.printf "%s" *)
  (*     (Structure.layout_refinements Undertype.pretty_layout refinements) *)
  (* in *)
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
