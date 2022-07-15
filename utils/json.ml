open Yojson.Basic.Util

let load_json fname =
  try Yojson.Basic.from_file fname
  with _ ->
    raise @@ failwith (Printf.sprintf "cannot find json file(%s)" fname)

let load_string j feild = j |> member feild |> to_string

let load_int j feild = j |> member feild |> to_int

let load_list j feild = j |> member feild |> to_list
