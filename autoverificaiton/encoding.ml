open Prop
open Zzdatatype.Datatype
module Ty = Normalty.Ast.T
open Normalty.Ast.Ntyped

module S = Map.Make (struct
  type t = lit list

  let compare a b =
    List.compare
      (fun a b -> Sexplib.Sexp.compare (sexp_of_lit a) (sexp_of_lit b))
      a b
end)

let stat_mp_app_in_prop prop =
  let rec aux m t =
    match t with
    | Lit _ -> m
    | MethodPred (mp, args) ->
        StrMap.update mp
          (fun s ->
            let s = match s with None -> S.empty | Some s -> s in
            Some (S.add args () s))
          m
    | Implies (e1, e2) -> aux (aux m e1) e2
    | Ite (e1, e2, e3) -> aux (aux (aux m e1) e2) e3
    | Not e -> aux m e
    | And es -> List.fold_left aux m es
    | Or es -> List.fold_left aux m es
    | Iff (e1, e2) -> aux (aux m e1) e2
    | Forall (_, _) | Exists (_, _) -> failwith "die"
  in
  aux StrMap.empty prop

let lit_has_uqvs uqvs = function
  | ACint _ | ACbool _ -> false
  | AVar x -> List.exists (fun y -> String.equal x.x y) uqvs
  | AOp2 (_, _, _) -> false

let subst_mp m prop =
  let rec aux prop =
    match prop with
    | Lit _ -> prop
    | MethodPred (mp, args) -> (
        match StrMap.find_opt m mp with
        | None -> prop
        | Some s -> (
            match S.find_opt args s with None -> prop | Some prop' -> prop'))
    | Implies (e1, e2) -> Implies (aux e1, aux e2)
    | Ite (e1, e2, e3) -> Ite (aux e1, aux e2, aux e3)
    | Not e -> Not (aux e)
    | And es -> And (List.map aux es)
    | Or es -> Or (List.map aux es)
    | Iff (e1, e2) -> Iff (aux e1, aux e2)
    | Forall (_, _) | Exists (_, _) -> failwith "die"
  in
  aux prop

let find_lits_with_vars uqvs prop =
  let m = stat_mp_app_in_prop prop in
  let has_uqv = List.exists (lit_has_uqvs uqvs) in
  let m =
    StrMap.filter_map
      (fun _ s ->
        let s = S.filter (fun args _ -> has_uqv args) s in
        if S.cardinal s == 0 then None else Some s)
      m
  in
  m

let layout_res m =
  StrMap.iter
    (fun k s ->
      Printf.printf "mp: %s\n" k;
      S.iter
        (fun lit _ ->
          Printf.printf "(%s)\n"
          @@ List.split_by_comma Frontend.pretty_layout_lit lit)
        s)
    m

let vars_reduction vars prop =
  let m = find_lits_with_vars vars prop in
  let m = StrMap.map (fun s -> S.map (fun _ -> mk_true) s) m in
  (* let () = layout_res m in *)
  let prop' = subst_mp m prop in
  (* let () = *)
  (*   Printf.printf "%s --> %s\n" *)
  (*     (Frontend.pretty_layout prop) *)
  (*     (Frontend.pretty_layout prop') *)
  (* in *)
  prop'

let uqv_encoding uqvs prop =
  let m = find_lits_with_vars uqvs prop in
  let m =
    StrMap.mapi
      (fun mp s ->
        let counter = ref 0 in
        S.mapi
          (fun lits _ ->
            let encoding =
              {
                ty = Ty.Ty_bool;
                x =
                  List.fold_left
                    (fun s lit ->
                      Printf.sprintf "%s,%s" s
                        (Frontend.lit_pretty_layout_ Frontend.psetting lit))
                    (Printf.sprintf "%s!%i" mp !counter)
                    lits;
              }
            in
            counter := !counter + 1;
            encoding)
          s)
      m
  in
  let new_uqvs =
    List.concat
    @@ List.map (fun s -> List.map snd @@ List.of_seq @@ S.to_seq s)
    @@ StrMap.to_value_list m
  in
  let prop =
    subst_mp (StrMap.map (fun s -> S.map (fun x -> Lit (AVar x)) s) m) prop
  in
  (new_uqvs, prop)
