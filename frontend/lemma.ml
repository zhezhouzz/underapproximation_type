open Ocaml_parser
open Parsetree
module L = Languages.Underty
module Ntyped = Languages.Ntyped
open Languages.Lemma
open Sugar

let undertype_of_ocamlexpr expr =
  let rec aux expr =
    match expr.pexp_desc with
    | Pexp_fun (_, _, id, e) ->
        let id =
          match Pat.patten_to_typed_ids id with
          | [ id ] -> id
          | _ -> failwith "wrong format"
        in
        let ids, prop = aux e in
        (id :: ids, prop)
    | _ -> ([], Autov.prop_of_ocamlexpr expr)
  in
  of_raw @@ aux expr

let pretty_layout lemma =
  (* let to_strings = List.map (fun x -> x.Languages.SMTtyped.x) in *)
  spf "%s." (Autov.pretty_layout_prop @@ to_prop lemma)

let print_with_lemma (pres, prop) =
  List.iter
    (fun pre ->
      Pp.printf "@{<bold>lemma: @} %s\n" @@ Autov.pretty_layout_prop pre)
    pres;
  Pp.printf "@{<bold>VC: @} %s\n" @@ Autov.pretty_layout_prop prop
