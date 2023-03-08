module T = struct
  open Sexplib.Std
  module NotatedT = Normalty.Ast.NotatedT

  type ty = NotatedT.t [@@deriving sexp]
  type id = string [@@deriving sexp]
  type 'a opttyped = { ty : ty option; x : 'a } [@@deriving sexp]
  type if_rec = bool [@@deriving sexp]
  type rankfunc = (string * Autov.Prop.lit) option [@@deriving sexp]

  type term =
    | Const of Value.t
    | Var of id
    | Tu of term opttyped list
    | Lam of ty * id * rankfunc * term opttyped
    | App of term opttyped * term opttyped list
    | Op of Op.T.op * term opttyped list
    | Let of if_rec * (ty * id) list * term opttyped * term opttyped
    | Ite of term opttyped * term opttyped * term opttyped
    | Match of term opttyped * case list
    | Exn

  and case = { constructor : id opttyped; args : id list; exp : term opttyped }
  [@@deriving sexp]

  let is_cobalt_fv_pattern str =
    let l = List.of_seq @@ String.to_seq str in
    let res =
      List.fold_left
        (fun res c ->
          match res with
          | None -> None
          | Some (None, None) ->
              if Core.Char.is_lowercase c then Some (Some [ c ], None) else None
          | Some (None, Some _) -> failwith "never happen"
          | Some (Some str, None) ->
              if Core.Char.is_lowercase c then Some (Some (str @ [ c ]), None)
              else if Core.Char.is_digit c then Some (Some str, Some [ c ])
              else None
          | Some (Some str, Some nstr) ->
              if Core.Char.is_digit c then Some (Some str, Some (nstr @ [ c ]))
              else None)
        (Some (None, None))
        l
    in
    match res with Some (Some _, Some _) -> true | _ -> false

  let show_fv idid init_ctx term =
    let fv_list = ref [] in
    let rec aux ctx { x; _ } =
      match x with
      | Const _ | Exn -> ()
      | Var id ->
          if List.exists (String.equal id) ctx then ()
          else fv_list := id :: !fv_list
      | Tu es -> List.iter (aux ctx) es
      | Lam (_, id, _, e) ->
          (* let () = *)
          (*   Printf.printf "id: %s\n" *)
          (*     (Zzdatatype.Datatype.StrList.to_string [ id ]) *)
          (* in *)
          aux (id :: ctx) e
      | App (e, es) ->
          aux ctx e;
          List.iter (aux ctx) es
      | Op (_, es) -> List.iter (aux ctx) es
      | Let (true, [ (_, x) ], rhs, body) ->
          aux (x :: ctx) rhs;
          aux (x :: ctx) body
      | Let (true, _, _, _) -> failwith "invalid term lang"
      | Let (false, lhs, rhs, body) ->
          let lhs = List.map (fun (_, x) -> x) lhs in
          let () = aux ctx rhs in
          aux (lhs @ ctx) body
      | Ite (e1, e2, e3) ->
          aux ctx e1;
          aux ctx e2;
          aux ctx e3
      | Match (e, cases) ->
          let () = aux ctx e in
          List.iter (fun { args; exp; _ } -> aux (args @ ctx) exp) cases
    in
    let () = aux init_ctx term in
    let fv_list = Zzdatatype.Datatype.List.slow_rm_dup String.equal !fv_list in
    let fv_list' = List.filter is_cobalt_fv_pattern fv_list in
    let () =
      if List.length fv_list' != 0 then
        (* Printf.printf "show_fv(%i):\n" idid; *)
        (* Printf.printf "fv_list: %s\n" *)
        (*   (Zzdatatype.Datatype.StrList.to_string fv_list); *)
        Printf.printf "func [%i]: %s\n" idid
          (Zzdatatype.Datatype.StrList.to_string fv_list')
      else ()
    in
    ()

  let make_untyped x = { ty = None; x }
  let make_untyped_var id = { ty = None; x = Var id }

  let make_untyped_tuple ids =
    { ty = None; x = Tu (List.map make_untyped_var ids) }

  let make_untyped_id_app (id, ids) =
    { ty = None; x = App (make_untyped_var id, List.map make_untyped_var ids) }

  let typedstr_to_var x = { ty = x.ty; x = Var x.x }
  let term_to_string_opt x = match x.x with Var name -> Some name | _ -> None

  let terms_to_strings_opt x =
    List.fold_left
      (fun l x ->
        match l with
        | None -> None
        | Some l -> (
            match term_to_string_opt x with
            | None -> None
            | Some x -> Some (l @ [ x ])))
      (Some []) x

  let erase_type term =
    let rec aux { x; _ } =
      let x =
        match x with
        | Const _ | Var _ | Exn -> x
        | Tu es -> Tu (List.map aux es)
        | Lam (ty, id, rankfunc, e) -> Lam (ty, id, rankfunc, aux e)
        | App (e, es) -> App (aux e, List.map aux es)
        | Op (op, es) -> Op (op, List.map aux es)
        | Let (if_rec, lhs, rhs, body) -> Let (if_rec, lhs, aux rhs, aux body)
        | Ite (e1, e2, e3) -> Ite (aux e1, aux e2, aux e3)
        | Match (e, cases) ->
            Match
              ( aux e,
                List.map
                  (fun { constructor; args; exp } ->
                    { constructor; args; exp = aux exp })
                  cases )
      in
      { ty = None; x }
    in
    aux term
end
