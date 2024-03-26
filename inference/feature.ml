open Language
open FrontendTyped
open Sugar

type t = Nt.t

type feature_tab = t lit list
(** Features are list of boolean typed literals *)

type feature_vec = bool list
type feature_vec_id = int
type label = Pos | Neg | Unknown

let is_not_neg = function Neg -> false | _ -> true
let is_positive = function Pos -> true | _ -> false

(** feature vec and feature vec id is one-to-one correspondence *)

let feature_vec_to_id vec =
  let rec aux = function
    | [] -> 0
    | true :: vec -> 1 + (2 * aux vec)
    | false :: vec -> 0 + (2 * aux vec)
  in
  aux vec

let feature_id_to_vec (num_features : int) id =
  let rec aux (n, res) id =
    if n == num_features then
      if id == 0 then res else _failatwith __FILE__ __LINE__ "die"
    else aux (n + 1, (id mod 2 == 1) :: res) (id / 2)
  in
  aux (0, []) id

let feature_vec_to_prop (ftab : feature_tab) vec =
  let props =
    List.map (fun (b, lit) ->
        let lit = lit #: Nt.bool_ty in
        if b then Lit lit else Not (Lit lit))
    @@ List.combine vec ftab
  in
  And props

let feature_id_to_prop (ftab : feature_tab) id =
  feature_vec_to_prop ftab @@ feature_id_to_vec (List.length ftab) id

(** make features from template (univerial quantified prop) *)

type template = { bvars : (t, string) typed list; body : t lit }

let rec destruct_univerial_prop = function
  | Forall { qv; body } ->
      let qvs, body = destruct_univerial_prop body in
      (qv :: qvs, body)
  | Lit lit -> ([], lit.x)
  | _ -> _failatwith __FILE__ __LINE__ "die"

let prop_to_template prop =
  let fvs = fv_prop prop in
  let () =
    if List.length fvs > 0 then _failatwith __FILE__ __LINE__ "die" else ()
  in
  let bvars, body = destruct_univerial_prop prop in
  { bvars; body }

open Zzdatatype.Datatype

let instantiate_template vars { bvars; body } =
  let vars_list =
    List.map (fun bvar -> List.filter (fun y -> Nt.eq bvar.ty y.ty) vars) bvars
  in
  let args_settings = List.choose_list_list vars_list in
  let args_settings = List.map (fun a -> List.combine bvars a) args_settings in
  let features =
    List.map
      (fun args_setting ->
        List.fold_right
          (fun (x, y) -> subst_lit_instance x.x (AVar y))
          args_setting body)
      args_settings
  in
  features

let mk_features templates vars =
  let vars = List.filter (fun x -> not (String.equal x.x "inv")) vars in
  let features =
    List.concat @@ List.map (instantiate_template vars) templates
  in
  let features =
    features
    @ List.filter_map
        (fun x -> match x.ty with Nt.Ty_bool -> Some (AVar x) | _ -> None)
        vars
  in
  let () =
    Env.show_debug_queries @@ fun _ ->
    Pp.printf "@{<bold>@{<orange>Features:@}@} %s\n"
      (List.split_by_comma layout_lit features)
  in
  features

let templates : template list option ref = ref None

let init_template props =
  let ts = List.map prop_to_template props in
  templates := Some ts

let get_template () =
  match !templates with
  | None -> _failatwith __FILE__ __LINE__ "die"
  | Some ts -> ts

open Base

let ___check_vec_to_id (vec : feature_vec) =
  let vec' = feature_id_to_vec (List.length vec) (feature_vec_to_id vec) in
  List.equal Bool.equal vec vec'

let%test "vec_to_id1" = ___check_vec_to_id [ true; false; true ]
let%test "vec_to_id2" = Int.equal 1 @@ feature_vec_to_id [ true; false; false ]
let%test "vec_to_id3" = Int.equal 2 @@ feature_vec_to_id [ false; true; false ]
let%test "vec_to_id4" = Int.equal 5 @@ feature_vec_to_id [ true; false; true ]
