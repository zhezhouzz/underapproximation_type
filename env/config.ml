open Sexplib.Std

type mode = Debug of string | Release
[@@deriving sexp]

type config = {mode: mode}
[@@deriving sexp]

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
  config := Some {mode}

let load_default () = load "../../../config/config.json"

let%test_unit "load_default" =
  let () = Printf.printf "%s\n" (Sys.getcwd()) in
  let () = load "../../../config/config.json" in
  match !config with
      | None -> failwith "empty config"
      | Some config ->
        match config.mode with
        | Debug ".log" -> ()
        | m -> failwith @@ Printf.sprintf "wrong mode: %s" @@ Sexplib.Sexp.to_string @@ sexp_of_mode m
