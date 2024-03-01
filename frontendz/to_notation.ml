open Ocaml5_parser
open Parsetree
open Sugar

let get_denoteopt_from_attr a =
  match a with [ x ] -> Some x.attr_name.txt | _ -> None

let get_denoteopt expr = get_denoteopt_from_attr expr.pexp_attributes

let get_denote expr =
  match get_denoteopt expr with
  | Some x -> x
  | None -> _failatwith __FILE__ __LINE__ ""

let get_pat_denoteopt pat = get_denoteopt_from_attr pat.ppat_attributes

let get_pat_denote expr =
  match get_pat_denoteopt expr with
  | Some x -> x
  | None -> _failatwith __FILE__ __LINE__ ""
