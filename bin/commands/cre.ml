open Core
open Caux
open Frontendz
open Zzdatatype.Datatype

let print_source_code meta_config_file source_file () =
  let code = Ocaml5_parser.Frontend.parse ~sourcefile:source_file in
  let code = List.map To_item.ocaml_structure_to_item code in
  Printf.printf "%s\n" (To_item.layout_structure code)

let print_source_code_raw =
  Command.basic ~summary:"print raw source code"
    Command.Let_syntax.(
      let%map_open meta_config_file = anon ("meta_config_file" %: regular_file)
      and source_file = anon ("source_code_file" %: regular_file) in
      print_source_code meta_config_file source_file)

let print_source_code =
  Command.group ~summary:"print source code" [ ("raw", print_source_code_raw) ]

let test =
  Command.group ~summary:"Poirot" [ ("print-source-code", print_source_code) ]
