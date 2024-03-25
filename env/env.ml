open Sexplib.Std

type mode = DebugTags of string list | Release [@@deriving sexp]

type prim_path = {
  normal_typing : string;
  coverage_typing : string;
  type_decls : string;
  axioms : string;
  templates : string;
}
[@@deriving sexp]

type meta_config = {
  mode : mode;
  max_printing_size : int;
  logfile : string;
  resfile : string;
  abdfile : string;
  num_quantifier : int;
  abd_templates : string list;
  prim_path : prim_path;
}
[@@deriving sexp]

type config = { all_mps : string list; underp : string; measure : string }
[@@deriving sexp]

let meta_config : meta_config option ref = ref None
let config : config option ref = ref None

let get_meta () =
  match !meta_config with None -> failwith "uninit" | Some config -> config

let get_mode () = (get_meta ()).mode
let get_max_printing_size () = (get_meta ()).max_printing_size

let show_log kw (f : unit -> unit) =
  match get_mode () with
  | DebugTags l when List.exists (String.equal kw) l -> f ()
  | _ -> ()

let show_debug_preprocess = show_log "preprocess"
let show_debug_result = show_log "result"
let show_debug_typing = show_log "typing"
let show_debug_queries = show_log "queries"
let show_debug_minterms = show_log "minterms"
let show_debug_solving = show_log "solving"
let show_debug_stat = show_log "stat"
let show_debug_info = show_log "info"
let show_debug_debug = show_log "debug"
let get_resfile () = (get_meta ()).resfile
let get_abdfile inputname = inputname ^ (get_meta ()).abdfile
let get_prim_path () = (get_meta ()).prim_path
let get_uninterops () = (get_meta ()).abd_templates

let get_measure () =
  match !config with
  | None -> failwith "uninited prim path"
  | Some config -> config.measure

open Json
open Yojson.Basic.Util

let load_meta meta_fname =
  let metaj = load_json meta_fname in
  let mode =
    match metaj |> member "mode" |> to_string with
    | "debug" ->
        DebugTags (metaj |> member "debug_tags" |> to_list |> List.map to_string)
    | "release" -> Release
    | _ -> failwith "config: unknown mode"
  in
  let max_printing_size = metaj |> member "max_printing_size" |> to_int in
  let resfile = metaj |> member "resfile" |> to_string in
  let logfile = metaj |> member "logfile" |> to_string in
  let abdfile = metaj |> member "abdfile" |> to_string in
  let abd_templates =
    metaj |> member "abd_templates" |> to_list |> List.map to_string
  in
  let num_quantifier = metaj |> member "num_quantifier" |> to_int in
  let p = metaj |> member "prim_path" in
  let prim_path =
    {
      templates = p |> member "templates" |> to_string;
      normal_typing = p |> member "normal_typing" |> to_string;
      coverage_typing = p |> member "coverage_typing" |> to_string;
      type_decls = p |> member "data_type_decls" |> to_string;
      axioms = p |> member "axioms" |> to_string;
    }
  in
  meta_config :=
    Some
      {
        mode;
        max_printing_size;
        prim_path;
        logfile;
        resfile;
        abdfile;
        abd_templates;
        num_quantifier;
      }
