module S = Languages.Termlang
module T = Languages.NormalAnormal
module Type = Languages.Normalty
open S
open Sugar

let get_tp e =
  match e.ty with
  | None ->
      (* Type.Ty_unit *)
      _failatwith __FILE__ __LINE__
        (spf "Never happen: untyped %s in the term language expr"
           (Frontend.Expr.layout e))
  | Some ty -> ty

let id_get_tp e =
  match e.ty with
  | None ->
      (* Type.Ty_unit *)
      _failatwith __FILE__ __LINE__
        (spf "Never happen: untyped %s in the term language expr" e.x)
  | Some ty -> ty

let id_trans (e : id opttyped) = T.{ ty = id_get_tp e; x = e.x }
let mk_t_term ty x = T.{ ty; x }

type cont = T.id T.typed -> T.term T.typed
type conts = T.id T.typed list -> T.term T.typed

let freshname () = Rename.unique "x"
let ret () x = T.{ ty = x.ty; x = V (Var x.x) }

(* let bind_value cont value = *)
(*   let x = freshname () in *)
(*   let body = cont T.{ ty = value.ty; x } in *)
(*   T.make_letval x value body *)

(* TODO: use value continuation *)

let force_naming name = match name with None -> freshname () | Some x -> x

let bind_value_with_name cont value x =
  let body = cont T.{ ty = value.ty; x } in
  T.make_letval x value body

(* TODO: alpha renaming *)
let rec to_anormal (e : term opttyped) (target_name : string option) :
    T.term T.typed =
  convert (ret ()) e target_name

and convert_multi (conts : conts) (es : term opttyped list)
    (target_names : string list option) : T.term T.typed =
  (* let convertings = *)
  (*   List.map (fun e -> ({ ty = get_tp e; x = freshname () }, e)) es *)
  (* in *)
  let target_names =
    match target_names with
    | None -> List.map (fun _ -> None) es
    | Some l -> List.map (fun x -> Some x) l
  in
  (List.fold_left
     (fun cont (rhs, target_name) names ->
       convert (fun name -> cont (name :: names)) rhs target_name)
     conts
  @@ List.combine es target_names)
    []

and convert (cont : cont) (e : term opttyped) (target_name : string option) :
    T.term T.typed =
  let ety = get_tp e in
  match e.x with
  | Const v ->
      bind_value_with_name cont
        T.{ ty = ety; x = Const v }
        (force_naming target_name)
  | Var id -> cont T.{ ty = ety; x = id }
  | Tu [] -> _failatwith __FILE__ __LINE__ "wrong tuple"
  | Tu [ e ] -> convert cont e target_name
  | Tu es ->
      convert_multi
        T.(
          fun args ->
            let tu = { ty = ety; x = force_naming target_name } in
            let body = cont tu in
            { ty = body.ty; x = LetTu { tu; args; body } })
        es None
  | Lam (ty, x, body) ->
      bind_value_with_name cont
        T.{ ty = ety; x = Lam ({ ty; x }, to_anormal body None) }
        (force_naming target_name)
  (* NOTE: zero arguments applicaition is still need a name *)
  (* | App (e, []) -> convert cont e target_name *)
  | App (e, es) ->
      convert
        T.(
          fun f ->
            convert_multi
              (fun args ->
                let ret = { ty = ety; x = force_naming target_name } in
                let body = cont ret in
                { ty = body.ty; x = LetApp { ret; f; args; body } })
              es None)
        e None
  | Let (false, lhs, rhs, body) -> (
      let body = convert cont body target_name in
      let lhs = List.map T.(fun (ty, x) -> { ty; x }) lhs in
      match lhs with
      | [] -> _failatwith __FILE__ __LINE__ ""
      | [ lhs ] ->
          convert
            (fun _ ->
              (* { *)
              (*   ty = body.ty; *)
              (*   x = LetVal { lhs; rhs = { ty = rhs.ty; x = Var rhs.x }; body }; *)
              (* } *)
              body)
            rhs (Some lhs.x)
      | _ ->
          convert
            (fun rhs ->
              { ty = body.ty; x = LetDeTu { args = lhs; tu = rhs; body } })
            rhs None)
  | Let (true, [ (ty, x) ], rhs, body) ->
      let body = convert cont body target_name in
      let f = T.{ ty; x } in
      convert
        T.(
          fun rhs ->
            {
              ty = body.ty;
              x =
                LetVal
                  {
                    lhs = f;
                    rhs =
                      { ty = f.ty; x = Fix (f, { ty = rhs.ty; x = Var rhs.x }) };
                    body;
                  };
            })
        rhs None
  | Let (true, _, _, _) -> _failatwith __FILE__ __LINE__ "invalid term lang"
  | Ite (e1, e2, e3) ->
      convert
        (fun x ->
          mk_t_term ety @@ T.Ite (x, to_anormal e2 None, to_anormal e3 None))
        e1 None
  | Match (e, cases) ->
      convert
        (fun x ->
          mk_t_term ety
          @@ T.Match
               ( x,
                 List.map
                   (fun case ->
                     {
                       T.constructor = case.constructor;
                       T.args = case.args;
                       T.exp = to_anormal case.exp None;
                     })
                   cases ))
        e None

let id_trans_rev (e : T.id T.typed) = { ty = Some e.T.ty; x = e.T.x }

let to_term e =
  let to_var id = { ty = Some id.T.ty; x = Var id.T.x } in
  let to_tid id = (id.T.ty, id.T.x) in
  let rec aux_value e =
    let x =
      match e.T.x with
      | T.Const v -> Const v
      | T.Var id -> Var id
      | T.Lam (x, body) -> Lam (x.ty, x.x, aux body)
      | T.Fix (_, body) -> (aux_value body).x
    in
    { ty = Some e.T.ty; x }
  and aux e =
    let x =
      match e.T.x with
      | V x -> (aux_value T.{ ty = e.ty; x }).x
      | T.LetTu { tu; args; body } ->
          Let
            ( false,
              [ to_tid tu ],
              { ty = Some tu.T.ty; x = Tu (List.map to_var args) },
              aux body )
      | T.LetDeTu { tu; args; body } ->
          Let (false, List.map to_tid args, to_var tu, aux body)
      | T.LetApp { ret; f; args; body } ->
          Let
            ( false,
              [ to_tid ret ],
              { ty = Some ret.T.ty; x = App (to_var f, List.map to_var args) },
              aux body )
      | T.LetVal { lhs; rhs; body } ->
          Let (false, [ to_tid lhs ], aux_value rhs, aux body)
      | T.Ite (e1, e2, e3) -> Ite (to_var e1, aux e2, aux e3)
      | T.Match (x, cases) ->
          Match
            ( to_var x,
              List.map
                (fun case ->
                  {
                    constructor = case.T.constructor;
                    args = case.T.args;
                    exp = aux case.T.exp;
                  })
                cases )
    in
    { ty = Some e.T.ty; x }
  in
  aux e
