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

type format = Raw | Typed | MonadicNormalForm

let split_source_code_ source_file outputdir =
  let open Languages in
  let code = Ocaml_parser.Frontend.parse ~sourcefile:source_file in
  List.iteri code ~f:(fun idx context ->
      let path = Printf.sprintf "%s/%i" outputdir idx in
      let () =
        match Sys_unix.file_exists path with
        | `Yes -> ()
        | `No -> Core_unix.mkdir path
        | `Unknown -> failwith "Unknown dir"
      in
      let oc = Out_channel.create (Printf.sprintf "%s/prog.ml" path) in
      Printf.fprintf oc "%s\n"
        (Ocaml_parser.Pprintast.string_of_structure [ context ]);
      Out_channel.close oc)

let split_source_code =
  Command.basic ~summary:"split a OCaml source code file into files"
    Command.Let_syntax.(
      let%map_open source_file = anon ("source_code_file" %: regular_file) in
      fun () ->
        let path =
          let tmp = String.split_on_chars source_file ~on:[ '/' ] in
          let path =
            match List.rev tmp with
            | [] -> ""
            | _ :: tmp ->
                Zzdatatype.Datatype.List.split_by "/" (fun x -> x)
                @@ List.rev tmp
          in
          path
        in
        (* let () = Printf.printf "path: %s\n" path in *)
        split_source_code_ source_file path)

let print_source_code meta_config_file source_file refine_file format () =
  let () = Env.load_meta meta_config_file in
  let () = Config.load_normal () in
  let notations, libs, refinements =
    Inputstage.load_user_defined_under_refinments refine_file
  in
  let open Typecheck in
  let open Languages in
  let ctx =
    NSimpleTypectx.(
      List.fold_left
        ~f:(fun ctx (x, ty) -> add_to_right ctx (x, UT.erase ty))
        ~init:empty libs)
  in
  let code = Ocaml_parser.Frontend.parse ~sourcefile:source_file in
  let msize = Env.get_max_printing_size () in
  let code = Struc.prog_of_ocamlstruct code in
  match format with
  | Raw ->
      let () = Printf.printf "[Raw]:\n\n%s\n\n" @@ Struc.layout code in
      ()
  | _ -> (
      let code = Termcheck.struc_check ctx code in
      match format with
      | Typed ->
          let () =
            Printf.printf "[(Basic) Typed]:\n\n%s\n\n" @@ Struc.layout code
          in
          ()
      | _ ->
          let code = Trans.struc_term_to_nan code in
          let () =
            Printf.printf "[(Basic) Typed Monadic Normal Form]:\n\n%s\n\n"
            @@ StrucNA.layout code
          in
          ())

let print_source_code_raw =
  Command.basic ~summary:"print raw source code"
    Command.Let_syntax.(
      let%map_open meta_config_file = anon ("meta_config_file" %: regular_file)
      and source_file = anon ("source_code_file" %: regular_file)
      and refine_file = anon ("coverage_type_file" %: regular_file) in
      print_source_code meta_config_file source_file refine_file Raw)

let print_source_code_typed =
  Command.basic ~summary:"print typed source code"
    Command.Let_syntax.(
      let%map_open meta_config_file = anon ("meta_config_file" %: regular_file)
      and source_file = anon ("source_code_file" %: regular_file)
      and refine_file = anon ("coverage_type_file" %: regular_file) in
      print_source_code meta_config_file source_file refine_file Typed)

let print_source_code_mnf =
  Command.basic ~summary:"print typed source code in MNF"
    Command.Let_syntax.(
      let%map_open meta_config_file = anon ("meta_config_file" %: regular_file)
      and source_file = anon ("source_code_file" %: regular_file)
      and refine_file = anon ("coverage_type_file" %: regular_file) in
      print_source_code meta_config_file source_file refine_file
        MonadicNormalForm)

let print_source_code =
  Command.group ~summary:"print source code"
    [
      ("raw", print_source_code_raw);
      ("typed", print_source_code_typed);
      ("mnf", print_source_code_mnf);
    ]

let print_coverage_types =
  Command.basic ~summary:"print coverage types from the given file"
    Command.Let_syntax.(
      let%map_open meta_config_file = anon ("meta_config_file" %: regular_file)
      and refine_file = anon ("coverage_type_file" %: regular_file) in
      fun () ->
        let () = Env.load_meta meta_config_file in
        let () = Config.load_normal () in
        let notations, libs, refinements =
          Inputstage.load_under_refinments refine_file
        in
        let () =
          if Int.equal 0 @@ List.length libs then ()
          else
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
  Command.basic ~summary:"coverage type check"
    Command.Let_syntax.(
      let%map_open meta_config_file = anon ("meta_config_file" %: regular_file)
      (* and config_file = anon ("config file" %: regular_file) *)
      and source_file = anon ("source_code_file" %: regular_file)
      and refine_file = anon ("coverage_type_file" %: regular_file) in
      fun () ->
        let () = Env.load_meta meta_config_file in
        let () = Config.load_normal () in
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
  Command.group ~summary:"Poirot"
    [
      ("print-coverage-types", print_coverage_types);
      ("coverage-type-check", under_type_check);
      ("print-source-code", print_source_code);
      ("split-source-code", split_source_code);
    ]

let%test_unit "rev" = [%test_eq: int list] (List.rev [ 3; 2; 1 ]) [ 1; 2; 3 ]
