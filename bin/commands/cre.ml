open Core
open Caux
open Languagez
open Zzdatatype.Datatype
open Preprocessing.Normal_item_typing
open To_item

let parse = Ocaml5_parser.Frontend.parse

let print_source_code meta_config_file source_file () =
  let () = Env.load_meta meta_config_file in
  let prim_path = Env.get_prim_path () in
  let s1 = parse ~sourcefile:prim_path.type_decls in
  let s2 = parse ~sourcefile:prim_path.normalp in
  let init_normal_ctx =
    struct_mk_ctx Typectx.emp (ocaml_structure_to_items (s1 @ s2))
  in
  let code =
    ocaml_structure_to_items
    @@ Ocaml5_parser.Frontend.parse ~sourcefile:source_file
  in
  let _ = Printf.printf "%s\n" (FrontendRaw.layout_structure code) in
  let _, code = struct_check init_normal_ctx code in
  let _ = Printf.printf "%s\n" (FrontendTyped.layout_structure code) in
  ()

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
