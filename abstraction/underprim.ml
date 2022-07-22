open Zzdatatype.Datatype
module NT = Languages.Normalty
module P = Autov.Prop
module T = Autov.Smtty

let tab_names = [ ("lt", "<"); ("intlistnil", "[]"); ("intlistcons", "::") ]
let rev_tab_names = [ ("rev_intlistnil", "[]"); ("rev_intlistcons", "::") ]

(* TODO: handle path *)
let tab, rev_tab =
  let open Frontend in
  let x =
    Ocaml_parser.Frontend.parse
      ~sourcefile:
        "/Users/zhezhou/workspace/research/underapproximation_type/data/prim/under.ml"
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
  let tab = get_tab tab_names in
  let rev_tab = get_tab rev_tab_names in
  (tab, rev_tab)

(* let tab = *)
(*   let open Languages.Underty in *)
(*   let open P in *)
(*   let mk_int_var name = { ty = T.Int; x = name } in *)
(*   let u, _, _ = Sugar.map3 mk_int_var ("_u", "_w", "_z") in *)
(*   [ *)
(*     ( "::", *)
(*       let tty h = *)
(*         make_basic *)
(*           NT.(Ty_list Ty_int) *)
(*           (fun t -> *)
(*             Forall *)
(*               ( u, *)
(*                 Implies *)
(*                   (MethodPred ("mem", [ t; u ]), MethodPred ("==", [ h; u ])) )) *)
(*       in *)
(*       let retty h _ = *)
(*         make_basic *)
(*           NT.(Ty_list Ty_int) *)
(*           (fun nu -> *)
(*             And *)
(*               [ *)
(*                 Forall *)
(*                   ( u, *)
(*                     Implies *)
(*                       ( MethodPred ("mem", [ nu; u ]), *)
(*                         MethodPred ("==", [ h; u ]) ) ); *)
(*                 Exists (u, MethodPred ("mem", [ nu; u ])); *)
(*               ]) *)
(*       in *)
(*       make_arrow "_h" (make_basic_top NT.Ty_int) (fun h -> *)
(*           make_arrow "_t" (tty h) (fun t -> retty h t)) ); *)
(*     ( "[]", *)
(*       make_basic *)
(*         NT.(Ty_list Ty_int) *)
(*         (fun nu -> Forall (u, Not (MethodPred ("mem", [ nu; u ])))) ); *)
(*     ( "<", *)
(*       make_arrow "_a" (make_basic_top NT.Ty_int) (fun a -> *)
(*           make_arrow "_b" *)
(*             (make_basic_top NT.(Ty_int)) *)
(*             (fun b -> *)
(*               make_basic *)
(*                 NT.(Ty_bool) *)
(*                 (fun nu -> Iff (Var nu, MethodPred ("<", [ a; b ]))))) ); *)
(*   ] *)

(* let rev_tab = *)

let m = StrMap.from_kv_list tab
let rev_m = StrMap.from_kv_list rev_tab
