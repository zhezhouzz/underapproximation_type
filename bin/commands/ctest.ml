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
        let x = Ocaml_parser.Frontend.parse ~sourcefile:source_file in
        let c = Structure.client_of_ocamlstruct x in
        let () = Printf.printf "%s" (Structure.layout c) in
        ())

let parse_to_typed_term =
  Command.basic ~summary:"parse_to_anormal"
    Command.Let_syntax.(
      let%map_open source_file = anon ("source file" %: regular_file) in
      fun () ->
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
        let code = Ocaml_parser.Frontend.parse ~sourcefile:source_file in
        let () =
          Printf.printf "%s\n\n"
          @@ Ocaml_parser.Pprintast.string_of_structure code
        in
        let code = Structure.client_of_ocamlstruct code in
        let () = Printf.printf "%s\n" @@ Structure.layout code in
        let code = Trans.struc_term_to_nan code in
        let () = Printf.printf "%s\n" (Na.struct_layout code) in
        ())

let test =
  Command.group ~summary:"test"
    [
      ("parse-to-anormal", parse_to_anormal);
      ("parse-to-typed-term", parse_to_typed_term);
      ("parsing-structure", parsing_structure);
    ]

let%test_unit "rev" = [%test_eq: int list] (List.rev [ 3; 2; 1 ]) [ 1; 2; 3 ]
