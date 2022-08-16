include Quantified.F (Normalty.T) (Underty.T)
open Sugar

let unify t1 t2 =
  let qbody, t2 = unify (t1.uqvs, t1.eqvs, t1.qbody, Underty.T.subst_id) t2 in
  ({ uqvs = t2.uqvs; eqvs = t2.eqvs; qbody }, t2)

let unifys ts target =
  List.fold_right
    (fun t (ts, target) ->
      let t', target = unify t target in
      (t' :: ts, target))
    ts ([], target)

let t_to_tuple_t = function
  | [] -> _failatwith __FILE__ __LINE__ ""
  | { uqvs; eqvs; qbody } :: t ->
      List.fold_left
        (fun res t_new ->
          let bodyt_new, { uqvs; eqvs; qbody } = unify t_new res in
          { uqvs; eqvs; qbody = Underty.T.join_tuple_t bodyt_new.qbody qbody })
        { uqvs; eqvs; qbody = UnderTy_tuple [ qbody ] }
        t

let disjunct_list_q = function
  | [] -> _failatwith __FILE__ __LINE__ ""
  | [ h ] -> h
  | h :: t ->
      let t, { uqvs; eqvs; qbody } = unifys t h in
      {
        uqvs;
        eqvs;
        qbody = Underty.T.disjunct_list (qbody :: List.map (fun x -> x.qbody) t);
      }

let conjunct_list_q = function
  | [] -> _failatwith __FILE__ __LINE__ ""
  | [ h ] -> h
  | h :: t ->
      let t, { uqvs; eqvs; qbody } = unifys t h in
      {
        uqvs;
        eqvs;
        qbody = Underty.T.conjunct_list (qbody :: List.map (fun x -> x.qbody) t);
      }

let eq t1 t2 =
  match subtract t1 t2 with
  | [], [] -> Underty.T.eq t1.qbody t2.qbody
  | _ -> false

let distruct_tuple_t_opt = function
  | { uqvs; eqvs; qbody = UnderTy_tuple ts } ->
      Some (List.map (fun qbody -> { uqvs; eqvs; qbody }) ts)
  | _ -> None
