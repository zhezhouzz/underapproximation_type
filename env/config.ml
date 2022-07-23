open Sexplib.Std

type mode = Debug of string | Release [@@deriving sexp]

type prim_path = { overp : string; underp : string; normalp : string }
[@@deriving sexp]

type config = { mode : mode; prim_path : prim_path } [@@deriving sexp]

let config = ref None

open Json
open Yojson.Basic.Util

let load fname =
  let j = load_json fname in
  let mode =
    match j |> member "mode" |> to_string with
    | "debug" ->
        let logfile = j |> member "logfile" |> to_string in
        Debug logfile
    | "release" -> Release
    | _ -> failwith "config: unknown mode"
  in
  let p = j |> member "prim_path" in
  let prim_path =
    {
      overp = p |> member "overp" |> to_string;
      underp = p |> member "underp" |> to_string;
      normalp = p |> member "normalp" |> to_string;
    }
  in
  let open Abstraction in
  let () = Prim.init_under_prim prim_path.underp in
  config := Some { mode; prim_path }

let get_prim_path () =
  match !config with
  | None -> failwith "uninited prim path"
  | Some config -> config.prim_path

let load_default () = load "config/config.json"

let%test_unit "load_default" =
  let () = Printf.printf "%s\n" (Sys.getcwd ()) in
  let () = load "../../../config/config.json" in
  match !config with
  | None -> failwith "empty config"
  | Some config -> (
      match config.mode with
      | Debug ".log" -> ()
      | m ->
          failwith
          @@ Printf.sprintf "wrong mode: %s"
          @@ Sexplib.Sexp.to_string @@ sexp_of_mode m)
