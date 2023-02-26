open Sexplib.Std

type mode =
  | Debug of {
      show_preprocess : bool;
      show_typing : bool;
      show_queries : bool;
      show_solving : bool;
      show_stat : bool;
      show_info : bool;
    }
  | Release
[@@deriving sexp]

type prim_path = {
  normalp : string;
  overp : string;
  under_basicp : string;
  underp : string;
  rev_underp : string;
  type_decls : string;
  lemmas : string;
  functional_lemmas : string;
}
[@@deriving sexp]

type meta_config = {
  mode : mode;
  max_printing_size : int;
  logfile : string;
  resfile : string;
}
[@@deriving sexp]

type config = { all_mps : string list; prim_path : prim_path; measure : string }
[@@deriving sexp]

let meta_config : meta_config option ref = ref None
let config : config option ref = ref None

let get_mode () =
  match !meta_config with
  | None -> failwith "uninit"
  | Some config -> config.mode

let get_max_printing_size () =
  match !meta_config with
  | None -> failwith "uninit"
  | Some config -> config.max_printing_size

let show_debug_preprocess (f : unit -> unit) =
  match get_mode () with
  | Debug { show_preprocess; _ } when show_preprocess -> f ()
  | _ -> ()

let show_debug_typing (f : unit -> unit) =
  match get_mode () with
  | Debug { show_typing; _ } when show_typing -> f ()
  | _ -> ()

let show_debug_queries (f : unit -> unit) =
  match get_mode () with
  | Debug { show_queries; _ } when show_queries -> f ()
  | _ -> ()

let show_debug_solving (f : unit -> unit) =
  match get_mode () with
  | Debug { show_solving; _ } when show_solving -> f ()
  | _ -> ()

let show_debug_stat (f : unit -> unit) =
  match get_mode () with
  | Debug { show_stat; _ } when show_stat -> f ()
  | _ -> ()

let show_debug_info (f : unit -> unit) =
  match get_mode () with
  | Debug { show_info; _ } when show_info -> f ()
  | _ -> ()

let get_resfile () =
  match !meta_config with
  | None -> failwith "get_resfile"
  | Some config -> config.resfile

open Json
open Yojson.Basic.Util

let load_meta meta_fname =
  let metaj = load_json meta_fname in
  let mode =
    match metaj |> member "mode" |> to_string with
    | "debug" ->
        let get_bool field =
          metaj |> member "debug_info" |> member field |> to_bool
        in
        Debug
          {
            show_preprocess = get_bool "show_preprocess";
            show_typing = get_bool "show_typing";
            show_queries = get_bool "show_queries";
            show_solving = get_bool "show_solving";
            show_stat = get_bool "show_stat";
            show_info = get_bool "show_info";
          }
    | "release" -> Release
    | _ -> failwith "config: unknown mode"
  in
  let max_printing_size = metaj |> member "max_printing_size" |> to_int in
  let resfile = metaj |> member "resfile" |> to_string in
  let logfile = metaj |> member "logfile" |> to_string in
  meta_config := Some { mode; max_printing_size; logfile; resfile }
