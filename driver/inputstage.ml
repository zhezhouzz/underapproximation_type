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
  let msize = Env.get_max_printing_size () in
  let () =
    Env.show_debug_preprocess @@ fun _ ->
    Printf.printf "\n[Load ocaml program]:\n%s\n\n"
    @@ short_str msize
    @@ Ocaml_parser.Pprintast.string_of_structure code
  in
  let code = Struc.prog_of_ocamlstruct code in
  let () =
    Env.show_debug_preprocess @@ fun _ ->
    Printf.printf "[Before type check]:\n%s\n\n"
    @@ short_str msize @@ Struc.layout code
  in
  let code = Termcheck.struc_check ctx code in
  let () =
    Env.show_debug_preprocess @@ fun _ ->
    Printf.printf "[Typed program]:\n%s\n\n"
    @@ short_str msize @@ Struc.layout code
  in
  let code = Trans.struc_term_to_nan code in
  let () =
    Env.show_debug_preprocess @@ fun _ ->
    Printf.printf "[Typed A-normal from]:\n%s\n\n"
    @@ short_str msize (StrucNA.layout code)
  in
  (* let () = _failatwith __FILE__ __LINE__ "end" in *)
  code

let load_normal_refinements refine_file =
  let refinements =
    Struc.func_decl_of_ocamlstruct
      (Ocaml_parser.Frontend.parse ~sourcefile:refine_file)
  in
  refinements

let _load_under_refinments refinements =
  let refinements =
    Struc.refinement_of_ocamlstruct UT.undertype_of_ocamlexpr refinements
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
        | Frontend.Structure.NoExt, x -> (a, b, c @ [ (None, x) ])
        | Frontend.Structure.Inv info, x -> (a, b, c @ [ (Some info, x) ])
        | Frontend.Structure.LibraryExt, x -> (a, b @ [ x ], c)
        | Frontend.Structure.NotationExt "over", (name, ty) ->
            ( a
              @ [
                  ( name,
                    MMT.Ot
                      UT.(
                        let basename, normalty, prop =
                          assume_base __FILE__ __LINE__ ty
                        in
                        { basename; normalty; prop }) );
                ],
              b,
              c )
        | Frontend.Structure.NotationExt "under", (name, ty) ->
            (a @ [ (name, MMT.Ut (UtNormal ty)) ], b, c)
        | Frontend.Structure.NotationExt str, _ ->
            _failatwith __FILE__ __LINE__ @@ spf "unknown label: %s" str)
      ~init:([], [], []) refinements
  in
  let _ = List.map ~f:(fun x -> UT.stat @@ snd @@ snd x) refinements in
  (* let () = *)
  (*   Printf.printf "[Loading libs type]:\n%s" *)
  (*     (Struc.layout_refinements UT.pretty_layout libs) *)
  (* in *)
  (* let () = *)
  (*   Printf.printf "[Loading refinement type]:\n%s" *)
  (*     (Struc.layout_refinements UT.pretty_layout refinements) *)
  (* in *)
  (notations, libs, refinements)

let load_under_refinments refine_file =
  _load_under_refinments (Ocaml_parser.Frontend.parse ~sourcefile:refine_file)

(* the first struct is the "mps" *)
(* the rest of structs are the codes *)

let load_user_defined_mps source_file =
  match Ocaml_parser.Frontend.parse ~sourcefile:source_file with
  | [] -> failwith "method predicates list is expected"
  | e :: _ -> Struc.mps_of_ocamlstruct e

let load_user_defined_under_refinments refine_file =
  let randomness_refinements =
    Ocaml_parser.Frontend.parse ~sourcefile:(Env.get_randomp_path ())
  in
  let refinements =
    match Ocaml_parser.Frontend.parse ~sourcefile:refine_file with
    | [] -> failwith "method predicates list is expected"
    | _ :: code -> code
  in
  (* let () = *)
  (*   Printf.printf "load_under_refinments\n%s\n" *)
  (*     (Ocaml_parser.Pprintast.string_of_structure refinements) *)
  (* in *)
  (* let () = failwith "end" in *)
  _load_under_refinments (randomness_refinements @ refinements)

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
    Env.show_debug_preprocess @@ fun _ ->
    Printf.printf "[Loading type decls]:\n%s\n" (Typedec.layout type_decls)
  in
  type_decls
