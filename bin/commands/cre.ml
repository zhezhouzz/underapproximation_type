open Core
open Caux
open Syntax
open Zzdatatype.Datatype
open Preprocessing.Normal_item_typing
open Frontend_opt
open To_item
open Raw_term_to_anf

let parse = Ocaml5_parser.Frontend.parse

let preproress meta_config_file source_file () =
  let prim_path = Env.get_prim_path () in
  let s1 = parse ~sourcefile:prim_path.type_decls in
  let s2 = parse ~sourcefile:prim_path.normal_typing in
  let init_normal_ctx =
    struct_mk_ctx emp (ocaml_structure_to_items (s1 @ s2))
  in
  let code =
    ocaml_structure_to_items
    @@ Ocaml5_parser.Frontend.parse ~sourcefile:source_file
  in
  (* let _ = Pp.printf "%s\n" (Rawlang.layout_structure code) in *)
  let _, code = struct_check init_normal_ctx code in
  (* let _ = Pp.printf "%s\n" (Typedlang.layout_structure code) in *)
  let code = normalize_structure code in
  (* let _ = Pp.printf "%s\n" (Typedlang.layout_structure code) in *)
  code

let print_source_code meta_config_file source_file () =
  let () = Env.load_meta meta_config_file in
  let _ = preproress meta_config_file source_file () in
  ()

let rec_arg = "rec_arg"

(* let handle_template templates = *)
(*   let rec_arg, templates = *)
(*     List.partition (fun x -> String.equal x.x rec_arg) *)
(*     @@ Typing.Itemcheck.gather_props templates *)
(*   in *)
(*   let Typing.Itemcheck.gather_props templates *)

(*   let rec_arg = *)
(*     match rec_arg with *)
(*     | [ x ] -> x.ty *)
(*     | _ -> failwith "cannot find builtin rec arg constraints" *)
(*   in *)
(*   let () = Typing.Termcheck.init_rec_arg rec_arg in *)
(*   let templates = List.map (fun x -> x.ty) templates in *)
(*   templates *)

(* let handle_lemma axioms = *)
(*   let axioms = *)
(*     List.map (fun x -> x.ty) @@ Typing.Itemcheck.gather_props axioms *)
(*   in *)
(*   axioms *)

let load_predefined_prop meta_config_file prim_path =
  let open Env in
  let axioms = preproress meta_config_file prim_path.axioms () in
  let () = update_axioms (Typing.Itemcheck.gather_props axioms) in
  let templates = preproress meta_config_file prim_path.templates () in
  let () = update_templates (Typing.Itemcheck.gather_props templates) in
  let statements = preproress meta_config_file prim_path.statements () in
  let () = update_statements (Typing.Itemcheck.gather_props statements) in
  ()

let direct_check_ mode meta_config_file source_file () =
  let () = Env.load_meta meta_config_file in
  let code = preproress meta_config_file source_file () in
  let prim_path = Env.get_prim_path () in
  let predefine = preproress meta_config_file prim_path.coverage_typing () in
  let builtin_ctx = Typing.Itemcheck.gather_uctx predefine in
  let () = load_predefined_prop meta_config_file prim_path in
  let _, rty1 = get_rty_by_name code "rty1" in
  let _, rty2 = get_rty_by_name code "rty2" in
  let ctx =
    Language.Rctx.{ builtin_ctx; local_ctx = emp; axioms = Env.get_axioms () }
  in
  match mode with
  | "subtyping" -> Subtyping.Subrty.external_check ctx (rty1, rty2)
  | "overlap" -> Subtyping.Overlaprty.external_check ctx (rty1, rty2)
  | _ -> failwith "unknown mode"

let type_check_ mode meta_config_file source_file () =
  let () = Env.load_meta meta_config_file in
  let code = preproress meta_config_file source_file () in
  let prim_path = Env.get_prim_path () in
  let predefine = preproress meta_config_file prim_path.coverage_typing () in
  let builtin_ctx = Typing.Itemcheck.gather_uctx predefine in
  let () = load_predefined_prop meta_config_file prim_path in
  let () = Inference.Feature.init_template (Env.get_templates ()) in
  let _ =
    Typing.Itemcheck.struc_check mode (Env.get_axioms (), builtin_ctx) code
  in
  ()

let print_erase_code meta_config_file source_file () =
  let () = Env.load_meta meta_config_file in
  let code =
    ocaml_structure_to_items
    @@ Ocaml5_parser.Frontend.parse ~sourcefile:source_file
  in
  let code = List.map item_erase code in
  let _ = Printf.printf "%s\n" (Language.Rawlang.layout_structure code) in
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
  let open Typing.Itemcheck in
  Command.group ~summary:"Poirot"
    [
      ("print-source-code", print_source_code);
      ("type-check", input_config_source "type check" (type_check_ TypeCheck));
      ("type-infer", input_config_source "type infer" (type_check_ TypeInfer));
      ("type-refine", input_config_source "type refine" (type_check_ TypeRefine));
      ( "subtype-check",
        input_config_source "subtype check" (direct_check_ "subtyping") );
      ( "overlap-check",
        input_config_source "overlaping check" (direct_check_ "overlap") );
    ]
