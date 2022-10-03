open Core
open Typecheck
open Languages
open Sugar

let load_ssa libs source_file =
  let ctx =
    NSimpleTypectx.(
      List.fold_left
        ~f:(fun ctx (x, ty) -> add_to_right ctx (x, UT.erase ty))
        ~init:empty libs)
  in
  let code = Ocaml_parser.Frontend.parse ~sourcefile:source_file in
  let () =
    Printf.printf "\n[Load ocaml program]:\n%s\n\n"
    @@ short_str 300
    @@ Ocaml_parser.Pprintast.string_of_structure code
  in
  let code = Struc.prog_of_ocamlstruct code in
  let () =
    Printf.printf "[Before type check]:\n%s\n\n"
    @@ short_str 300 @@ Struc.layout code
  in
  let code = Termcheck.struc_check ctx code in
  let () =
    Printf.printf "[Typed program]:\n%s\n\n"
    @@ short_str 300 @@ Struc.layout code
    (* (Struc.layout code) *)
  in
  let code = Trans.struc_term_to_nan code in
  let () =
    Printf.printf "[Typed A-normal from]:\n%s\n\n"
    @@ short_str 300 (StrucNA.layout code)
    (* (StrucNA.layout code) *)
  in
  code

let load_normal_refinements refine_file =
  let refinements =
    Struc.func_decl_of_ocamlstruct
      (Ocaml_parser.Frontend.parse ~sourcefile:refine_file)
  in
  (* let () = *)
  (*   Printf.printf "[Loading normal type]:\n%s\n\n" *)
  (*     (Struc.layout_normals refinements) *)
  (* in *)
  refinements

let load_over_refinments refine_file =
  let refinements =
    Struc.refinement_of_ocamlstruct OT.overtype_of_ocamlexpr
      (Ocaml_parser.Frontend.parse ~sourcefile:refine_file)
  in
  let refinements =
    List.map
      ~f:(fun ((a, name), ty) -> (a, (name, Overtycheck.infer ty)))
      refinements
  in
  let notations, _, refinements =
    List.fold_left
      ~f:(fun (a, b, c) x ->
        match x with
        | Frontend.Structure.NoExt, x -> (a, b, c @ [ x ])
        | Frontend.Structure.LibraryExt, x -> (a, b @ [ x ], c)
        | Frontend.Structure.NotationExt, x -> (a @ [ x ], b, c))
      ~init:([], [], []) refinements
  in
  let () =
    Printf.printf "[Loading notations type]:\n%s"
      (Struc.layout_refinements OT.pretty_layout notations)
  in
  (* let () = *)
  (*   Printf.printf "[Loading refinement type]:\n%s" *)
  (*     (Struc.layout_refinements OT.pretty_layout refinements) *)
  (* in *)
  refinements

let load_under_refinments refine_file =
  let refinements =
    Struc.refinement_of_ocamlstruct UT.undertype_of_ocamlexpr
      (Ocaml_parser.Frontend.parse ~sourcefile:refine_file)
  in
  let refinements =
    List.map
      ~f:(fun ((a, name), ty) -> (a, (name, Undertycheck.infer [] ty)))
      refinements
  in
  let notations, libs, refinements =
    List.fold_left
      ~f:(fun (a, b, c) x ->
        match x with
        | Frontend.Structure.NoExt, x -> (a, b, c @ [ x ])
        | Frontend.Structure.LibraryExt, x -> (a, b @ [ x ], c)
        | Frontend.Structure.NotationExt, x -> (a @ [ x ], b, c))
      ~init:([], [], []) refinements
  in
  (* let () = *)
  (*   Printf.printf "[Loading libs type]:\n%s" *)
  (*     (Struc.layout_refinements UT.pretty_layout libs) *)
  (* in *)
  (* let () = *)
  (*   Printf.printf "[Loading refinement type]:\n%s" *)
  (*     (Struc.layout_refinements UT.pretty_layout refinements) *)
  (* in *)
  (notations, libs, refinements)

open Languages
module LA = Lemma

let load_lemmas lemma_file =
  let lemmas =
    Struc.refinement_of_ocamlstruct Lemma.undertype_of_ocamlexpr
      (Ocaml_parser.Frontend.parse ~sourcefile:lemma_file)
  in
  let lemmas = List.map ~f:(fun ((_, name), lemma) -> (name, lemma)) lemmas in
  (* let () = *)
  (*   Printf.printf "[Loading Lemmas type]:\n%s" *)
  (*     (sprintf "Lemma %s" @@ Struc.layout_refinements Lemma.pretty_layout lemmas) *)
  (* in *)
  lemmas

let load_type_decls refine_file =
  let x = Ocaml_parser.Frontend.parse ~sourcefile:refine_file in
  let type_decls = Struc.type_decl_of_ocamlstruct x in
  let () =
    Printf.printf "[Loading type decls]:\n%s\n" (Typedec.layout type_decls)
  in
  type_decls
