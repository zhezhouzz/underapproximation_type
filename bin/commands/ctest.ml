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
        let code =
          Typecheck.Termcheck.struc_check Languages.NSimpleTypectx.empty code
        in
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
        let code =
          Typecheck.Termcheck.struc_check Languages.NSimpleTypectx.empty code
        in
        let () = Printf.printf "%s\n" @@ Structure.layout code in
        let code = Trans.struc_term_to_nan code in
        let () =
          Printf.printf "%s\n" (Structure.layout @@ Trans.struc_nan_to_term code)
        in
        ())

(* let parsing_over_refinements = *)
(*   Command.basic ~summary:"parsing_over_refinements" *)
(*     Command.Let_syntax.( *)
(*       let%map_open refine_file = anon ("source file" %: regular_file) in *)
(*       fun () -> *)
(*         let () = Config.load_default () in *)
(*         let x = Ocaml_parser.Frontend.parse ~sourcefile:refine_file in *)
(*         let refinements = *)
(*           List.map ~f:(fun ((_, a), b) -> (a, b)) *)
(*           @@ Structure.refinement_of_ocamlstruct Overty.overtype_of_ocamlexpr x *)
(*         in *)
(*         let () = *)
(*           Printf.printf "%s" *)
(*             (Structure.layout_refinements Overty.pretty_layout refinements) *)
(*         in *)
(*         ()) *)

let print_coverage_types =
  Command.basic ~summary:"parsing_under_refinements"
    Command.Let_syntax.(
      let%map_open meta_config_file = anon ("meta config file" %: regular_file)
      and refine_file = anon ("source file" %: regular_file) in
      fun () ->
        let () = Env.load_meta meta_config_file in
        let notations, libs, refinements =
          Inputstage.load_under_refinments refine_file
        in
        let () =
          Pp.printf "@{<bold>Library Function Types:@}\n%s"
            Languages.(Struc.layout_refinements UT.pretty_layout libs)
        in
        let () =
          Pp.printf "@{<bold>Types to Check:@}\n%s"
            Languages.(
              Struc.layout_refinements UT.pretty_layout
              @@ List.map ~f:snd refinements)
        in
        ())

let parsing_type_decls =
  Command.basic ~summary:"parsing_type_decls"
    Command.Let_syntax.(
      let%map_open refine_file = anon ("source file" %: regular_file) in
      fun () ->
        let () = Config.load_default () in
        let x = Ocaml_parser.Frontend.parse ~sourcefile:refine_file in
        let type_decls = Structure.type_decl_of_ocamlstruct x in
        let () = Printf.printf "%s\n" (Typedec.layout type_decls) in
        ())

(* let over_type_check = *)
(*   Command.basic ~summary:"over_type_check" *)
(*     Command.Let_syntax.( *)
(*       let%map_open source_file = anon ("source file" %: regular_file) *)
(*       and refine_file = anon ("refine_file" %: regular_file) in *)
(*       fun () -> *)
(*         let () = Config.load_default () in *)
(*         let code = Inputstage.load_ssa [] source_file in *)
(*         let refinements = Inputstage.load_over_refinments refine_file in *)
(*         let () = Printf.printf "[Type checking]:\n" in *)
(*         let code = Typecheck.Overcheck.struc_check code refinements in *)
(*         let () = Printf.printf "[Type check]: OK\n" in *)
(*         ()) *)

let under_type_check =
  Command.basic ~summary:"under_type_check"
    Command.Let_syntax.(
      let%map_open meta_config_file = anon ("meta config file" %: regular_file)
      (* and config_file = anon ("config file" %: regular_file) *)
      and source_file = anon ("source file" %: regular_file)
      and refine_file = anon ("refine_file" %: regular_file) in
      fun () ->
        let () = Env.load_meta meta_config_file in
        let () = Config.load refine_file in
        let notations, libs, refinements =
          Inputstage.load_user_defined_under_refinments refine_file
        in
        let code = Inputstage.load_ssa libs source_file in
        let () = Typecheck.Undersub.subtyping_check_counter_set0 () in
        (* let () = failwith "end" in *)
        let runtime, results =
          Sugar.clock (fun () ->
              Typecheck.Undercheck.struc_check code notations libs refinements)
        in
        let str =
          match Ast.StrucNA.stat code with
          | [ (name, num_branches, num_localvars) ] -> (
              let num_query = Typecheck.Undersub.(!subtyping_check_counter) in
              let str =
                Printf.sprintf "%s & $%i$ & $%i$ & " name num_branches
                  num_localvars
              in
              match refinements with
              | [ (_, (_, ty)) ] ->
                  let num_mps = List.length @@ Config.get_mps () in
                  (* let num_mps, _ = Ast.UT.stat ty in *)
                  Printf.sprintf
                    "& %s$%i$ & $%i$ & $(%i, %i)$ & $%0.2f(%0.2f)$\n" str
                    num_mps num_query
                    Typecheck.Undersub.(!max_uqvs_num)
                    Typecheck.Undersub.(!max_eqvs_num)
                    runtime
                    (runtime /. float_of_int num_query)
              | _ -> "")
          | _ -> ""
        in
        let () =
          match results with
          | [ res ] ->
              let oc = Out_channel.create @@ Env.get_resfile () in
              Printf.fprintf oc "%b %s\n" res str;
              Out_channel.close oc
          | _ -> ()
        in
        ())

(* let under_subtype_check = *)
(*   Command.basic ~summary:"under_type_check" *)
(*     Command.Let_syntax.( *)
(*       let%map_open config_file = anon ("config file" %: regular_file) *)
(*       and refine_file = anon ("refine_file" %: regular_file) *)
(*       and left = anon ("left name" %: string) *)
(*       and right = anon ("right name" %: string) in *)
(*       fun () -> *)
(*         let () = Config.load config_file in *)
(*         let _, _, refinements = Inputstage.load_under_refinments refine_file in *)
(*         let () = Typecheck.Inv_check.struc_check refinements (left, right) in *)
(*         ()) *)

(* let under_post_shrink = *)
(*   Command.basic ~summary:"under_post_shrink" *)
(*     Command.Let_syntax.( *)
(*       let%map_open config_file = anon ("config file" %: regular_file) *)
(*       and source_file = anon ("source file" %: regular_file) *)
(*       and refine_file = anon ("refine_file" %: regular_file) *)
(*       and infer_ctx_file = anon ("infer_ctx_file" %: regular_file) in *)
(*       fun () -> *)
(*         let () = Config.load config_file in *)
(*         let notations, libs, refinements = *)
(*           Inputstage.load_under_refinments refine_file *)
(*         in *)
(*         let code = Inputstage.load_ssa libs source_file in *)
(*         let notations = failwith "zz" in *)
(*         let res = *)
(*           Inference.Infer.struc_post_shrink infer_ctx_file code notations libs *)
(*             refinements *)
(*         in *)
(*         let () = *)
(*           List.iter res ~f:(fun (idx, name, uty, res) -> *)
(*               let () = *)
(*                 Pp.printf "@{<bold>Task %i@}: %s\n%s\n" idx name *)
(*                   (Languages.UT.pretty_layout uty) *)
(*               in *)
(*               let () = Inference.Check_false.(print_res res) in *)
(*               ()) *)
(*         in *)
(*         ()) *)

(* let test_mk_features = *)
(*   Command.basic ~summary:"test_mk_features" *)
(*     Command.Let_syntax.( *)
(*       let%map_open source_file = anon ("source file" %: regular_file) *)
(*       and infer_ctx_file = anon ("infer_ctx_file" %: regular_file) in *)
(*       fun () -> *)
(*         let () = Config.load_default () in *)
(*         let code = Inputstage.load_ssa [] source_file in *)
(*         let settings = *)
(*           List.map *)
(*             ~f: *)
(*               Languages.StrucNA.( *)
(*                 fun { name; body } -> *)
(*                   let args, ret = Languages.UL.get_args_return_name "v" body in *)

(*                   (name, (List.map ~f:(fun x -> (x, x)) args, ret))) *)
(*             code *)
(*         in *)
(*         (\* let () = *\) *)
(*         (\*   Pp.printf "@{<bold>len@}(settings) = %i\n" @@ List.length settings *\) *)
(*         (\* in *\) *)
(*         (\* let () = failwith (Printf.sprintf "end: %i" @@ List.length settings) in *\) *)
(*         let () = *)
(*           List.iter *)
(*             ~f:(fun (name, (args, retv)) -> *)
(*               let infer_ctx = *)
(*                 Inference.Infer_ctx.load infer_ctx_file args retv *)
(*               in *)
(*               let () = Inference.Infer_ctx.print infer_ctx in *)
(*               ()) *)
(*             settings *)
(*         in *)
(*         ()) *)

let init =
  Command.basic ~summary:"init"
    Command.Let_syntax.(
      let%map_open source_file = anon ("source file" %: regular_file)
      and refine_file = anon ("refine_file" %: regular_file) in
      fun () ->
        let () = Config.load_default () in
        ())

let qcheck =
  Command.basic ~summary:"init"
    Command.Let_syntax.(
      let%map_open num = anon ("num" %: int) in
      fun () ->
        let open Sugar in
        let () = Printf.printf "QCheck\n" in
        (* let () = Cgen.test num in *)
        ())

let test =
  Command.group ~summary:"test"
    [
      ("parse-to-anormal", parse_to_anormal);
      ("parse-to-typed-term", parse_to_typed_term);
      ("parse-structure", parsing_structure);
      (* ("parse-over-refinements", parsing_over_refinements); *)
      ("print-coverage-types", print_coverage_types);
      ("parsing-type-decls", parsing_type_decls);
      (* ("over-type-check", over_type_check); *)
      ("under-type-check", under_type_check);
      (* ("under-post-shrink", under_post_shrink); *)
      (* ("test-mk-features", test_mk_features); *)
      ("qcheck", qcheck);
      ("init", init) (* ("under-subtype-check", under_subtype_check); *);
    ]

let%test_unit "rev" = [%test_eq: int list] (List.rev [ 3; 2; 1 ]) [ 1; 2; 3 ]
