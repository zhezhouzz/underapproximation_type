open Core
open Frontend
open Typecheck

let load_ssa source_file =
  let ctx = Languages.NSimpleTypectx.empty in
  let code = Ocaml_parser.Frontend.parse ~sourcefile:source_file in
  let () =
    Printf.printf "\n[Load ocaml program]:\n%s\n\n"
    @@ Ocaml_parser.Pprintast.string_of_structure code
  in
  let code = Structure.client_of_ocamlstruct code in
  let () =
    Printf.printf "[Before type check]:\n%s\n\n" @@ Structure.layout code
  in
  let code = Termcheck.struc_check ctx code in
  let () = Printf.printf "[Typed program]:\n%s\n\n" @@ Structure.layout code in
  let code = Trans.struc_term_to_nan code in
  let () =
    Printf.printf "[Typed A-normal from]:\n%s\n\n"
      (Structure.layout @@ Trans.struc_nan_to_term code)
  in
  (* let _ = failwith "end" in *)
  code

let load_normal_refinements refine_file =
  let refinements =
    Structure.func_decl_of_ocamlstruct
      (Ocaml_parser.Frontend.parse ~sourcefile:refine_file)
  in
  let () =
    Printf.printf "[Loading normal type]:\n%s\n\n"
      (Structure.layout_normals refinements)
  in
  refinements

let load_over_refinments refine_file =
  let refinements =
    Structure.refinement_of_ocamlstruct Overty.overtype_of_ocamlexpr
      (Ocaml_parser.Frontend.parse ~sourcefile:refine_file)
  in
  let refinements =
    List.map
      ~f:(fun ((a, name), ty) -> (a, (name, Overtycheck.infer ty)))
      refinements
  in
  let notations, refinements =
    Sugar.map2 (List.map ~f:snd) @@ List.partition_tf ~f:fst refinements
  in
  let () =
    Printf.printf "[Loading notations type]:\n%s"
      (Structure.layout_refinements Overty.pretty_layout notations)
  in
  let () =
    Printf.printf "[Loading refinement type]:\n%s"
      (Structure.layout_refinements Overty.pretty_layout refinements)
  in
  refinements

let load_under_refinments refine_file =
  let refinements =
    Structure.refinement_of_ocamlstruct
      Qunderty.quantified_undertype_of_ocamlexpr
      (Ocaml_parser.Frontend.parse ~sourcefile:refine_file)
  in
  let refinements =
    List.map
      ~f:
        Languages.Qunderty.(
          fun ((a, name), { qvs; qbody }) ->
            (a, (name, { qvs; qbody = Undertycheck.infer qvs qbody })))
      refinements
  in
  let notations, refinements =
    Sugar.map2 (List.map ~f:snd) @@ List.partition_tf ~f:fst refinements
  in
  let () =
    Printf.printf "[Loading notations type]:\n%s"
      (Structure.layout_refinements Qunderty.pretty_layout notations)
  in
  let () =
    Printf.printf "[Loading refinement type]:\n%s"
      (Structure.layout_refinements Qunderty.pretty_layout refinements)
  in
  (notations, refinements)

module UT = Languages.Underty
module QUT = Languages.Qunderty
module LA = Languages.Lemma
module NT = Languages.Ntyped

let load_lemmas lemma_file =
  let lemmas =
    Structure.refinement_of_ocamlstruct
      Qunderty.quantified_undertype_of_ocamlexpr
      (Ocaml_parser.Frontend.parse ~sourcefile:lemma_file)
  in
  let rec ty_to_uprop =
    UT.(
      function
      | UnderTy_base { prop; _ } -> ([], prop)
      | UnderTy_arrow { argname; argty; retty; _ } ->
          let uqvs, prop = ty_to_uprop retty in
          (NTyped.{ x = argname; ty = erase argty } :: uqvs, prop)
      | _ -> failwith "die")
  in
  let lemmas =
    List.map
      ~f:
        Languages.Lemma.(
          fun ((_, name), qty) ->
            let eqvs, (uqvs, prop) = QUT.(qty.qvs, ty_to_uprop qty.qbody) in
            (name, { qvs = eqvs; qbody = { qvs = uqvs; qbody = prop } }))
      lemmas
  in
  let () =
    Printf.printf "[Loading Lemmas type]:\n%s"
      (sprintf "Lemma %s"
      @@ Structure.layout_refinements Lemma.pretty_layout lemmas)
  in
  lemmas

let load_type_decls refine_file =
  let x = Ocaml_parser.Frontend.parse ~sourcefile:refine_file in
  let type_decls = Structure.type_decl_of_ocamlstruct x in
  let () =
    Printf.printf "[Loading type decls]:\n%s\n" (Typedec.layout type_decls)
  in
  type_decls
