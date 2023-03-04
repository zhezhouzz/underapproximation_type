open Ast
module T = Termlang
open Zzdatatype.Datatype

let remove_dummy_eq e =
  let open NL in
  let rec aux_value e =
    match e.x with
    | Exn | Lit _ | Var _ -> e
    | Lam { lamarg; lambody } ->
        { ty = e.ty; x = Lam { lamarg; lambody = aux lambody } }
    | Fix { fixname; fstarg; lambody } ->
        { ty = e.ty; x = Fix { fixname; fstarg; lambody = aux lambody } }
  and aux e =
    (* let () = Printf.printf "%s\n" @@ layout e in *)
    let x =
      match e.x with
      | V v -> V (aux_value v)
      | LetApp { ret; f; args; body } ->
          LetApp { ret; f; args; body = aux body }
      | LetDtConstructor { ret; f; args; body } ->
          LetDtConstructor { ret; f; args; body = aux body }
      | LetOp { ret; op; args; body } ->
          LetOp { ret; op; args; body = aux body }
      | LetTu { tu; args; body } -> LetTu { tu; args; body = aux body }
      | LetDeTu { tu; args; body } -> LetDeTu { tu; args; body = aux body }
      | LetVal { lhs; rhs; body } -> (
          let rhs = aux_value rhs in
          match rhs.x with
          | Var id' ->
              (* let () = Printf.printf "subst: %s -> %s\n" lhs.x id' in *)
              (aux (subst_id (lhs.x, id'.x) body)).x
          | _ -> LetVal { lhs; rhs; body = aux body })
      | Ite { cond; e_t; e_f } -> Ite { cond; e_t = aux e_t; e_f = aux e_f }
      | Match { matched; cases } ->
          Match
            {
              matched;
              cases =
                List.map (fun case -> { case with exp = aux case.exp }) cases;
            }
    in
    { ty = e.ty; x }
  in
  aux e

let remove_dummy_let e =
  let open NL in
  (* let () = Printf.printf "start simplify\n" in *)
  let rec aux_value e =
    match e.x with
    | Exn | Lit _ | Var _ -> e
    | Lam { lamarg; lambody } ->
        { ty = e.ty; x = Lam { lamarg; lambody = aux lambody } }
    | Fix { fixname; fstarg; lambody } ->
        { ty = e.ty; x = Fix { fixname; fstarg; lambody = aux lambody } }
  and aux e =
    (* let () = *)
    (*   Printf.printf "working on: %s\n" *)
    (*   @@ Frontend.Anormal.layout Term2normalanormal.to_term e *)
    (* in *)
    let x =
      match e.x with
      | V v -> V (aux_value v)
      | LetApp { ret; f; args; body } ->
          LetApp { ret; f; args; body = aux body }
      | LetDtConstructor { ret; f; args; body } ->
          LetDtConstructor { ret; f; args; body = aux body }
      | LetOp { ret; op; args; body } ->
          LetOp { ret; op; args; body = aux body }
      | LetTu { tu; args; body } -> LetTu { tu; args; body = aux body }
      | LetDeTu { tu; args; body } -> LetDeTu { tu; args; body = aux body }
      | LetVal { lhs; rhs; body } -> (
          let body = aux body in
          match body.x with
          | V { x = Var id'; _ } when String.equal lhs.x id'.x ->
              V (aux_value rhs)
          | _ -> LetVal { lhs; rhs = aux_value rhs; body })
      | Ite { cond; e_t; e_f } -> Ite { cond; e_t = aux e_t; e_f = aux e_f }
      | Match { matched; cases } ->
          Match
            {
              matched;
              cases =
                List.map (fun case -> { case with exp = aux case.exp }) cases;
            }
    in
    { ty = e.ty; x }
  in
  (* aux e *)
  aux (remove_dummy_eq e)

let simplify e = remove_dummy_let e
