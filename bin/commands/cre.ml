open Core
open Caux
open Lang
open Zzdatatype.Datatype
open Preprocessing.Normal_item_typing
open To_item
open Raw_term_to_anf

let parse = Ocaml5_parser.Frontend.parse

let preproress meta_config_file source_file () =
  let prim_path = Env.get_prim_path () in
  let s1 = parse ~sourcefile:prim_path.type_decls in
  let s2 = parse ~sourcefile:prim_path.normal_typing in
  let init_normal_ctx =
    struct_mk_ctx Typectx.emp (ocaml_structure_to_items (s1 @ s2))
  in
  let code =
    ocaml_structure_to_items
    @@ Ocaml5_parser.Frontend.parse ~sourcefile:source_file
  in
  (* let _ = Pp.printf "%s\n" (Rawlang.layout_structure code) in *)
  let _, code = struct_check init_normal_ctx code in
  (* let _ = Pp.printf "%s\n" (Typedlang.layout_structure code) in *)
  let code = normalize_structure code in
  let _ = Pp.printf "%s\n" (Typedlang.layout_structure code) in
  code

let print_source_code meta_config_file source_file () =
  let () = Env.load_meta meta_config_file in
  let _ = preproress meta_config_file source_file () in
  ()

let type_check_ meta_config_file source_file () =
  let () = Env.load_meta meta_config_file in
  let code = preproress meta_config_file source_file () in
  let prim_path = Env.get_prim_path () in
  let predefine = preproress meta_config_file prim_path.coverage_typing () in
  let builtin_ctx = Typing.Itemcheck.gather_uctx predefine in
  let axioms = preproress meta_config_file prim_path.axioms () in
  let axioms =
    List.map (fun x -> x.ty) @@ Typing.Itemcheck.gather_axioms axioms
  in
  let _, rty1 = get_rty_by_name code "rty1" in
  let _, rty2 = get_rty_by_name code "rty2" in
  let ctx = Typedlang.{ builtin_ctx; local_ctx = emp; axioms } in
  let res = Subtyping.Subrty.sub_rty_bool ctx (rty1, rty2) in
  let () =
    Typedlang.pprint_typectx_subtyping
      (fun _ -> Typedlang.pprint_typectx emp)
      (rty1, rty2)
  in
  let () = Pp.printf "Result: %b\n" res in
  ()

let rec_arg = "rec_arg"

let handle_template templates =
  let rec_arg, templates =
    List.partition (fun x -> String.equal x.x rec_arg)
    @@ Typing.Itemcheck.gather_axioms templates
  in
  let rec_arg =
    match rec_arg with
    | [ x ] -> x.ty
    | _ -> failwith "cannot find builtin rec arg constraints"
  in
  let () = Typing.Termcheck.init_rec_arg rec_arg in
  let templates = List.map (fun x -> x.ty) templates in
  templates

let handle_lemma axioms =
  let axioms =
    List.map (fun x -> x.ty) @@ Typing.Itemcheck.gather_axioms axioms
  in
  axioms

let type_check_ meta_config_file source_file () =
  let () = Env.load_meta meta_config_file in
  let code = preproress meta_config_file source_file () in
  let prim_path = Env.get_prim_path () in
  let predefine = preproress meta_config_file prim_path.coverage_typing () in
  let builtin_ctx = Typing.Itemcheck.gather_uctx predefine in
  let axioms = preproress meta_config_file prim_path.axioms () in
  let axioms = handle_lemma axioms in
  let templates = preproress meta_config_file prim_path.templates () in
  let templates = handle_template templates in
  let () = Inference.Feature.init_template templates in
  let _ = Typing.Itemcheck.struc_check (axioms, builtin_ctx) code in
  ()

let type_infer_ meta_config_file source_file () =
  let () = Env.load_meta meta_config_file in
  let code = preproress meta_config_file source_file () in
  let prim_path = Env.get_prim_path () in
  let predefine = preproress meta_config_file prim_path.coverage_typing () in
  let builtin_ctx = Typing.Itemcheck.gather_uctx predefine in
  let axioms = preproress meta_config_file prim_path.axioms () in
  let axioms = handle_lemma axioms in
  let templates = preproress meta_config_file prim_path.templates () in
  let templates =
    List.map (fun x -> x.ty) @@ Typing.Itemcheck.gather_axioms templates
  in
  let () = Inference.Feature.init_template templates in
  let _ = Typing.Itemcheck.struc_infer (axioms, builtin_ctx) code in
  ()

let print_erase_code meta_config_file source_file () =
  let () = Env.load_meta meta_config_file in
  let code =
    ocaml_structure_to_items
    @@ Ocaml5_parser.Frontend.parse ~sourcefile:source_file
  in
  let code = List.map item_erase code in
  let _ = Printf.printf "%s\n" (Rawlang.layout_structure code) in
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
