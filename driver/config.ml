open Json
open Yojson.Basic.Util
open Env

let __concat_without_overlap msg eq l1 l2 =
  List.fold_left
    (fun res x ->
      if List.exists (fun y -> eq x y) res then
        failwith (Printf.sprintf "__concat_without_overlap: %s" msg)
      else x :: res)
    l1 l2

let load fname =
  let j = load_json fname in
  let p = j |> member "prim_path" in
  let prim_path =
    {
      normalp = p |> member "normalp" |> to_string;
      overp = p |> member "overp" |> to_string;
      under_basicp = p |> member "under_basicp" |> to_string;
      underp = p |> member "underp" |> to_string;
      rev_underp = p |> member "rev_underp" |> to_string;
      type_decls = p |> member "type_decls" |> to_string;
      lemmas = p |> member "lemmas" |> to_string;
      functional_lemmas = p |> member "functional_lemmas" |> to_string;
    }
  in
  let open Abstraction in
  let all_mps = j |> member "all_mps" |> to_list |> List.map to_string in
  let measure = j |> member "measure" |> to_string in
  let under_basicr =
    match Inputstage.load_under_refinments prim_path.under_basicp with
    | [], underr, [] -> underr
    | _, _, _ -> failwith "wrong under prim"
  in
  let underr =
    match Inputstage.load_under_refinments prim_path.underp with
    | [], underr, [] -> underr
    | _, _, _ -> failwith "wrong under prim"
  in
  let underr =
    under_basicr @ underr
    (* __concat_without_overlap "basic underp is overlapped with underp" *)
    (*   (fun (x, _) (y, _) -> String.equal x y) *)
    (*   under_basicr underr *)
  in
  let rev_underr =
    match Inputstage.load_under_refinments prim_path.rev_underp with
    | [], underr, [] -> underr
    | _, _, _ -> failwith "wrong under prim"
  in
  let lemmas = Inputstage.load_lemmas prim_path.lemmas in
  let lemmas =
    List.filter (fun (_, lemma) -> Lemma.filter_by_mps all_mps lemma) lemmas
  in
  let functional_lemmas = Inputstage.load_lemmas prim_path.functional_lemmas in
  let functional_lemmas =
    List.filter
      (fun (_, lemma) -> Lemma.filter_by_mps all_mps lemma)
      functional_lemmas
  in
  (* let () = failwith "end" in *)
  let () =
    Prim.init
      ( Inputstage.load_type_decls prim_path.type_decls,
        Inputstage.load_normal_refinements prim_path.normalp,
        (* Inputstage.load_over_refinments prim_path.overp, *)
        [],
        underr,
        rev_underr,
        lemmas,
        functional_lemmas )
  in
  config := Some { all_mps; prim_path; measure }

let get_mps () =
  match !config with
  | None -> failwith "uninited prim path"
  | Some config -> config.all_mps

let get_prim_path () =
  match !config with
  | None -> failwith "uninited prim path"
  | Some config -> config.prim_path

let load_default () =
  let () = load_meta "../../../meta-config.json" in
  load "../../../config/config.json"

let%test_unit "load_default" =
  let () = Printf.printf "%s\n" (Sys.getcwd ()) in
  let () = load_meta "../../../meta-config.json" in
  let () = load "../../../config/config.json" in
  match !meta_config with
  | None -> failwith "empty config"
  | Some meta_config -> (
      match meta_config.mode with
      | Debug _ -> ()
      | m ->
          failwith
          @@ Printf.sprintf "wrong mode: %s"
          @@ Sexplib.Sexp.to_string @@ sexp_of_mode m)
