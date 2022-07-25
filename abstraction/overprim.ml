open Zzdatatype.Datatype
module NT = Languages.Normalty
module P = Autov.Prop
module T = Autov.Smtty

let tab_names = [ ("lt", "<"); ("intlistnil", "[]"); ("intlistcons", "::") ]
let m = ref None

let make_m (refinements : (string * Languages.Overty.t) list) =
  let get_tab tab =
    List.map
      (fun (idx, name) ->
        match List.find_opt (fun (x, _) -> String.equal idx x) refinements with
        | None -> failwith "die: get over prim tab"
        | Some (_, ty) -> (name, ty))
      tab
  in
  m := Some (StrMap.from_kv_list @@ get_tab tab_names)

(* let over_tab = *)
(*   let open Languages.Overty in *)
(*   [ *)
(*     ( "::", *)
(*       let prop h t nu = *)
(*         P.( *)
(*           mk_forall_intqv "u" (fun u -> *)
(*               Iff *)
(*                 ( Or [ mk_mp_vars "mem" [ t; u ]; mk_mp_vars "==" [ h; u ] ], *)
(*                   mk_mp_vars "mem" [ nu; u ] ))) *)
(*       in *)
(*       make_arrow "_h" NT.Ty_int make_basic_top (fun h -> *)
(*           make_arrow "_t" *)
(*             NT.(Ty_list Ty_int) *)
(*             make_basic_top *)
(*             (fun t -> *)
(*               make_basic "_nu" NT.(Ty_list Ty_int) (fun nu -> prop h t nu))) ); *)
(*     ( "[]", *)
(*       make_basic "_nu" *)
(*         NT.(Ty_list Ty_int) *)
(*         (fun nu -> *)
(*           P.(mk_forall_intqv "u" (fun u -> Not (mk_mp_vars "mem" [ nu; u ])))) *)
(*     ); *)
(*     ( "<", *)
(*       make_arrow "_a" NT.Ty_int make_basic_top (fun a -> *)
(*           make_arrow "_b" NT.Ty_int make_basic_top (fun b -> *)
(*               make_basic "_nu" NT.Ty_bool (fun nu -> *)
(*                   P.(Iff (Var nu, mk_mp_vars "<" [ a; b ]))))) ); *)
(*   ] *)

(* let m = StrMap.from_kv_list over_tab *)
