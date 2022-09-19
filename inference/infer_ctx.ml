open Languages
open Ntyped

type arg = { codearg : string typed; tyarg : string typed }

type t = {
  qvs : string typed list;
  inpargs : arg list;
  retv : string typed;
  mps : string list;
  features : (string * string typed list) list;
  code_features : (string * string typed list) list;
}

open Zzdatatype.Datatype
open Sugar
module P = Autov.Prop

let layout_ntyped x = spf "(%s:%s)" x.x @@ Normalty.Frontend.layout x.ty
let args_to_codeargs = List.map (fun x -> x.codearg)
let args_to_tyarg = List.map (fun x -> x.tyarg)

let get_code_features infer_ctx =
  let m =
    StrMap.from_kv_list
    @@ List.map
         (fun { codearg; tyarg } -> (tyarg.x, codearg.x))
         infer_ctx.inpargs
  in
  List.map
    (fun (mp, args) ->
      ( mp,
        List.map
          (fun x ->
            match StrMap.find_opt m x.x with
            | Some y -> { x = y; ty = x.ty }
            | None -> x)
          args ))
    infer_ctx.features

let feature_to_prop (mp, args) =
  let open P in
  let lits = List.map (fun x -> AVar x) args in
  MethodPred (mp, lits)

let layout_feature feature = Autov.pretty_layout_prop @@ feature_to_prop feature

let print { qvs; inpargs; retv; mps; features; code_features } =
  Pp.printf "@{<bold>qvs:@} %s;\n" (List.split_by_comma layout_ntyped qvs);
  Pp.printf "@{<bold>code:@} (%s);\n"
    (List.split_by_comma layout_ntyped @@ args_to_codeargs inpargs);
  Pp.printf "@{<bold>ty:@} (%s) -> %s;\n"
    (List.split_by_comma layout_ntyped @@ args_to_codeargs inpargs)
    (layout_ntyped retv);
  Pp.printf "@{<bold>mps:@} %s;\n" (List.split_by_comma (fun x -> x) mps);
  Pp.printf "@{<bold>features:@} %s;\n"
    (List.split_by_comma layout_feature features);
  Pp.printf "@{<bold>code features:@} %s;\n"
    (List.split_by_comma layout_feature code_features)

let args_sort l =
  List.sort
    (fun a b ->
      Sexplib.Sexp.compare
        (sexp_of_typed Sexplib.Std.sexp_of_string a)
        (sexp_of_typed Sexplib.Std.sexp_of_string b))
    l

let mk_feature_mp qvs args mp =
  let tuples =
    match mp with
    | "==" ->
        let tuples = List.choose_n qvs 2 in
        let tuples' = List.choose_list_list [ args; qvs ] in
        let tuples =
          List.filter
            (fun l ->
              match l with
              | h :: [ t ] ->
                  NT.is_basic_tp h.ty && eq h.ty t.ty
                  && not (String.equal h.x t.x)
              | _ -> _failatwith __FILE__ __LINE__ "")
            (tuples @ tuples')
        in
        List.slow_rm_dup (List.equal typed_eq) (List.map args_sort tuples)
    | "mem" | "hd" ->
        let dtargs, _ = List.partition (fun x -> NT.is_dt x.ty) args in
        let tuples = List.choose_list_list_order [ dtargs; qvs ] in
        (* let () = *)
        (*   List.iter *)
        (*     (fun t -> *)
        (*       Pp.printf "tuples : %s\n" (List.split_by_comma layout_ntyped t)) *)
        (*     tuples *)
        (* in *)
        List.slow_rm_dup (List.equal typed_eq) tuples
    | "empty" ->
        let dtargs, _ = List.partition (fun x -> NT.is_dt x.ty) args in
        List.map (fun x -> [ x ]) dtargs
    | _ -> _failatwith __FILE__ __LINE__ ""
  in
  List.map (fun tuple -> (mp, tuple)) tuples

let init_features t = t
let get_code_inpout x = args_to_codeargs x.inpargs @ [ x.retv ]
let get_ty_inpout x = args_to_tyarg x.inpargs @ [ x.retv ]

let init ~qvs ~inpargs ~retv ~mps =
  let features =
    List.concat
    @@ List.map (mk_feature_mp qvs (args_to_tyarg inpargs @ [ retv ])) mps
  in
  let code_features =
    List.concat
    @@ List.map (mk_feature_mp qvs (args_to_codeargs inpargs @ [ retv ])) mps
  in
  { qvs; inpargs; retv; mps; features; code_features }

open Json
open Yojson.Basic.Util

let default_qvs =
  [
    { ty = Ty_int; x = "u" }; { ty = Ty_int; x = "w" }; { ty = Ty_int; x = "z" };
  ]

let load fname inpargs retv =
  let inpargs = List.map (fun (codearg, tyarg) -> { codearg; tyarg }) inpargs in
  let j = load_json fname in
  let qvnum = j |> member "qvnum" |> to_int in
  let mps = List.map (fun j -> to_string j) (j |> member "mps" |> to_list) in
  let qvs =
    if qvnum > List.length default_qvs then
      _failatwith __FILE__ __LINE__ "unimp"
    else List.sublist default_qvs ~start_included:0 ~end_excluded:qvnum
  in
  init ~qvs ~inpargs ~retv ~mps
