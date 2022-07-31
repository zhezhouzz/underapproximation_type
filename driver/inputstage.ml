open Core
open Frontend
open Typecheck

let load_ssa source_file =
  let code = Ocaml_parser.Frontend.parse ~sourcefile:source_file in
  let () =
    Printf.printf "%s\n\n" @@ Ocaml_parser.Pprintast.string_of_structure code
  in
  let code = Structure.client_of_ocamlstruct code in
  let () = Printf.printf "%s\n" @@ Structure.layout code in
  let code = Termcheck.struc_check code in
  let () = Printf.printf "%s\n" @@ Structure.layout code in
  let code = Trans.struc_term_to_nan code in
  let () =
    Printf.printf "[Loading typed A-normal from]:\n%s\n"
      (Structure.layout @@ Trans.struc_nan_to_term code)
  in
  (* let _ = failwith "end" in *)
  code

let load_over_refinments refine_file =
  let refinements =
    Structure.refinement_of_ocamlstruct Overtype.overtype_of_ocamlexpr
      (Ocaml_parser.Frontend.parse ~sourcefile:refine_file)
  in
  let refinements =
    List.map ~f:(fun (name, ty) -> (name, Overtycheck.infer ty)) refinements
  in
  let () =
    Printf.printf "[Loading refinement type]:\n%s"
      (Structure.layout_refinements Overtype.pretty_layout refinements)
  in
  refinements

let load_under_refinments refine_file =
  let refinements =
    Structure.refinement_of_ocamlstruct Undertype.undertype_of_ocamlexpr
      (Ocaml_parser.Frontend.parse ~sourcefile:refine_file)
  in
  let refinements =
    List.map ~f:(fun (name, ty) -> (name, Undertycheck.infer ty)) refinements
  in
  let () =
    Printf.printf "[Loading refinement type]:\n%s"
      (Structure.layout_refinements Undertype.pretty_layout refinements)
  in
  refinements
