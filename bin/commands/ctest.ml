open Core
open Caux
open Frontend

(* let parsing_signature = *)
(*   Command.basic ~summary:"parsing signature" *)
(*     Command.Let_syntax.( *)
(*       let%map_open source_file = anon ("source file" %: regular_file) in *)
(*       fun () -> *)
(*         let x = Frontend.parse ~sourcefile:source_file in *)
(*         let si = Parsing.Signature.signature_of_ocamlstructs x in *)
(*         let () = Printf.printf "%s" (Signature.layout si) in *)
(*         ()) *)

let parsing_structure =
  Command.basic ~summary:"parsing structure"
    Command.Let_syntax.(
      let%map_open source_file = anon ("source file" %: regular_file) in
      fun () ->
        let () = Config.load_default () in
        let x = Ocaml_parser.Frontend.parse ~sourcefile:source_file in
        let c = Structure.client_of_ocamlstruct x in
        let () = Printf.printf "%s" (Structure.layout c) in
        ())

let parse_to_typed_term =
  Command.basic ~summary:"parse_to_anormal"
    Command.Let_syntax.(
      let%map_open source_file = anon ("source file" %: regular_file) in
      fun () ->
        let () = Config.load_default () in
        let code = Ocaml_parser.Frontend.parse ~sourcefile:source_file in
        let () =
          Printf.printf "%s\n\n"
          @@ Ocaml_parser.Pprintast.string_of_structure code
        in
        let code = Structure.client_of_ocamlstruct code in
        let () = Printf.printf "%s\n" @@ Structure.layout code in
        let code = Typecheck.Termcheck.struc_check code in
        let () = Printf.printf "%s\n" @@ Structure.layout code in
        ())

let parse_to_anormal =
  Command.basic ~summary:"parse_to_anormal"
    Command.Let_syntax.(
      let%map_open source_file = anon ("source file" %: regular_file) in
      fun () ->
        let () = Config.load_default () in
        let code = Ocaml_parser.Frontend.parse ~sourcefile:source_file in
        let () =
          Printf.printf "%s\n\n"
          @@ Ocaml_parser.Pprintast.string_of_structure code
        in
        let code = Structure.client_of_ocamlstruct code in
        let () = Printf.printf "%s\n" @@ Structure.layout code in
        let code = Typecheck.Termcheck.struc_check code in
        let () = Printf.printf "%s\n" @@ Structure.layout code in
        let code = Trans.struc_term_to_nan code in
        let () =
          Printf.printf "%s\n" (Structure.layout @@ Trans.struc_nan_to_term code)
        in
        ())

let parsing_over_refinements =
  Command.basic ~summary:"parsing_over_refinements"
    Command.Let_syntax.(
      let%map_open refine_file = anon ("source file" %: regular_file) in
      fun () ->
        let () = Config.load_default () in
        let x = Ocaml_parser.Frontend.parse ~sourcefile:refine_file in
        let refinements =
          Structure.refinement_of_ocamlstruct Overtype.overtype_of_ocamlexpr x
        in
        let () =
          Printf.printf "%s"
            (Structure.layout_refinements Overtype.pretty_layout refinements)
        in
        ())

let parsing_under_refinements =
  Command.basic ~summary:"parsing_over_refinements"
    Command.Let_syntax.(
      let%map_open refine_file = anon ("source file" %: regular_file) in
      fun () ->
        let () = Config.load_default () in
        let x = Ocaml_parser.Frontend.parse ~sourcefile:refine_file in
        let refinements =
          Structure.refinement_of_ocamlstruct Undertype.undertype_of_ocamlexpr x
        in
        let () =
          Printf.printf "%s"
            (Structure.layout_refinements Undertype.pretty_layout refinements)
        in
        ())

let over_type_check =
  Command.basic ~summary:"over_type_check"
    Command.Let_syntax.(
      let%map_open source_file = anon ("source file" %: regular_file)
      and refine_file = anon ("refine_file" %: regular_file) in
      fun () ->
        let () = Config.load_default () in
        let code = Ocaml_parser.Frontend.parse ~sourcefile:source_file in
        let () =
          Printf.printf "%s\n\n"
          @@ Ocaml_parser.Pprintast.string_of_structure code
        in
        let code = Structure.client_of_ocamlstruct code in
        let () = Printf.printf "%s\n" @@ Structure.layout code in
        let code = Typecheck.Termcheck.struc_check code in
        let () = Printf.printf "%s\n" @@ Structure.layout code in
        let code = Trans.struc_term_to_nan code in
        let () =
          Printf.printf "%s\n" (Structure.layout @@ Trans.struc_nan_to_term code)
        in
        let refinements =
          Structure.refinement_of_ocamlstruct Overtype.overtype_of_ocamlexpr
            (Ocaml_parser.Frontend.parse ~sourcefile:refine_file)
        in
        let () =
          Printf.printf "%s"
            (Structure.layout_refinements Overtype.pretty_layout refinements)
        in
        let code = Typecheck.Overcheck.struc_check code refinements in
        ())

let under_type_check =
  Command.basic ~summary:"under_type_check"
    Command.Let_syntax.(
      let%map_open source_file = anon ("source file" %: regular_file)
      and refine_file = anon ("refine_file" %: regular_file) in
      fun () ->
        let () = Config.load_default () in
        let code = Ocaml_parser.Frontend.parse ~sourcefile:source_file in
        let code = Structure.client_of_ocamlstruct code in
        let code = Typecheck.Termcheck.struc_check code in
        let code = Trans.struc_term_to_nan code in
        let () =
          Printf.printf "%s\n" (Structure.layout @@ Trans.struc_nan_to_term code)
        in
        let refinements =
          Structure.refinement_of_ocamlstruct Undertype.undertype_of_ocamlexpr
            (Ocaml_parser.Frontend.parse ~sourcefile:refine_file)
        in
        let () =
          Printf.printf "%s"
            (Structure.layout_refinements Undertype.pretty_layout refinements)
        in
        let code = Typecheck.Undercheck.struc_check code refinements in
        ())

let test =
  Command.group ~summary:"test"
    [
      ("parse-to-anormal", parse_to_anormal);
      ("parse-to-typed-term", parse_to_typed_term);
      ("parse-structure", parsing_structure);
      ("parse-over-refinements", parsing_over_refinements);
      ("parse-under-refinements", parsing_under_refinements);
      ("over-type-check", over_type_check);
      ("under-type-check", under_type_check);
    ]

let%test_unit "rev" = [%test_eq: int list] (List.rev [ 3; 2; 1 ]) [ 1; 2; 3 ]
