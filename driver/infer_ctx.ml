open Json
open Yojson.Basic.Util
open Inference.Infer_ctx
open Sugar
open Languages.Ntyped
open Zzdatatype.Datatype

let default_qvs =
  [
    { ty = Ty_int; x = "u" }; { ty = Ty_int; x = "w" }; { ty = Ty_int; x = "z" };
  ]

let load fname args =
  let j = load_json fname in
  let qvnum = j |> member "qvnum" |> to_int in
  let mps = List.map (fun j -> to_string j) (j |> member "mps" |> to_list) in
  let qvs =
    if qvnum > List.length default_qvs then
      _failatwith __FILE__ __LINE__ "unimp"
    else List.sublist default_qvs ~start_included:0 ~end_excluded:qvnum
  in
  init ~qvs ~args ~mps
