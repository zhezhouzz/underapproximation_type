open Core
open Caux
open Language
open Zzdatatype.Datatype
open Preprocessing.Normal_item_typing
open To_item
open Raw_term_to_anf

let parse = Ocaml5_parser.Frontend.parse

let preproress meta_config_file source_file () =
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
  (* let _ = Pp.printf "%s\n" (FrontendRaw.layout_structure code) in *)
  let _, code = struct_check init_normal_ctx code in
  (* let _ = Pp.printf "%s\n" (FrontendTyped.layout_structure code) in *)
  let code = normalize_structure code in
  let _ = Pp.printf "%s\n" (FrontendTyped.layout_structure code) in
  code

let print_source_code meta_config_file source_file () =
  let () = Env.load_meta meta_config_file in
  let _ = preproress meta_config_file source_file () in
  ()

let type_check_ meta_config_file source_file () =
  let () = Env.load_meta meta_config_file in
  let code = preproress meta_config_file source_file () in
  let prim_path = Env.get_prim_path () in
  let predefine = preproress meta_config_file prim_path.under_basicp () in
  let builtin_ctx = Typing.Itemcheck.gather_uctx predefine in
  let lemmas = preproress meta_config_file prim_path.lemmas () in
  let lemmas =
    List.map (fun x -> x.ty) @@ Typing.Itemcheck.gather_axioms lemmas
  in
  (* let axioms = Typing.Itemcheck.gather_axioms predefine in *)
  let _ = Typing.Itemcheck.struc_check (lemmas, builtin_ctx) code in
  ()

let type_infer_ meta_config_file source_file () =
  let () = Env.load_meta meta_config_file in
  let code = preproress meta_config_file source_file () in
  let prim_path = Env.get_prim_path () in
  let predefine = preproress meta_config_file prim_path.under_basicp () in
  let builtin_ctx = Typing.Itemcheck.gather_uctx predefine in
  let lemmas = preproress meta_config_file prim_path.lemmas () in
  let lemmas =
    List.map (fun x -> x.ty) @@ Typing.Itemcheck.gather_axioms lemmas
  in
  (* let axioms = Typing.Itemcheck.gather_axioms predefine in *)
  let _ = Typing.Itemcheck.struc_infer (lemmas, builtin_ctx) code in
  ()

let print_erase_code meta_config_file source_file () =
  let () = Env.load_meta meta_config_file in
  let code =
    ocaml_structure_to_items
    @@ Ocaml5_parser.Frontend.parse ~sourcefile:source_file
  in
  let code = List.map item_erase code in
  let _ = Printf.printf "%s\n" (FrontendRaw.layout_structure code) in
  ()

let input_config_source message f =
  Command.basic ~summary:message
    Command.Let_syntax.(
      let%map_open meta_config_file = anon ("meta_config_file" %: regular_file)
      and source_file = anon ("source_code_file" %: regular_file) in
      f meta_config_file source_file)

let print_source_code =
  Command.group ~summary:"print source code"
    [
      ("raw", input_config_source "print raw source code" print_source_code);
      ("erase", input_config_source "print erase source code" print_erase_code);
    ]

let test =
  Command.group ~summary:"Poirot"
    [
      ("print-source-code", print_source_code);
      ("type-check", input_config_source "type check" type_check_);
      ("type-infer", input_config_source "type infer" type_infer_);
    ]
