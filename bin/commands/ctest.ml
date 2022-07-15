open Core
open Caux
open Ocaml_parser

(* let parsing_signature = *)
(*   Command.basic ~summary:"parsing signature" *)
(*     Command.Let_syntax.( *)
(*       let%map_open source_file = anon ("source file" %: regular_file) in *)
(*       fun () -> *)
(*         let x = Frontend.parse ~sourcefile:source_file in *)
(*         let si = Parsing.Signature.signature_of_ocamlstructs x in *)
(*         let () = Printf.printf "%s" (Signature.layout si) in *)
(*         ()) *)

(* let parsing_structure = *)
(*   Command.basic ~summary:"parsing structure" *)
(*     Command.Let_syntax.( *)
(*       let%map_open source_file = anon ("source file" %: regular_file) in *)
(*       fun () -> *)
(*         let x = Frontend.parse ~sourcefile:source_file in *)
(*         let c = Parsing.Structure.client_of_ocamlstruct x in *)
(*         let () = Printf.printf "%s" (Client.layout c) in *)
(*         ()) *)

let test =
  Command.group ~summary:"test"
    [
      (* ("parsing-signature", parsing_signature); *)
      (* ("parsing-structure", parsing_structure); *)
    ]


let%test_unit "rev" =
  [%test_eq: int list] (List.rev [ 3; 2; 1 ]) [ 1; 2; 3 ]
