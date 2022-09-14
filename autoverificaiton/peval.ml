open Ast
open Sugar

let peval_lit lit =
  let rec aux lit =
    match lit with
    | ACbool _ | ACint _ | AVar _ -> lit
    | AOp2 (op, a, b) -> (
        let a, b = map2 aux (a, b) in
        match (op, a, b) with
        | "+", ACint i, ACint j -> ACint (i + j)
        | "+", _, _ ->
            if Sexplib.Sexp.compare (sexp_of_lit a) (sexp_of_lit b) < 0 then
              AOp2 (op, a, b)
            else AOp2 (op, b, a)
        | "-", ACint i, ACint j -> ACint (i - j)
        | _, _, _ -> AOp2 (op, a, b))
  in
  aux lit

let add_to_sets e es =
  if
    List.exists
      (fun e' -> Sexplib.Sexp.compare (sexp_of_t e) (sexp_of_t e') == 0)
      es
  then es
  else e :: es

let merge_sets es1 es2 = List.fold_right add_to_sets es1 es2

let peval prop =
  let rec aux = function
    | Lit lit -> Lit (peval_lit lit)
    | MethodPred (mp, args) -> (
        let args = List.map peval_lit args in
        match (mp, args) with
        | "==", [ ACbool false; x ] | "==", [ x; ACbool false ] ->
            aux (Not (Lit x))
        | "==", [ ACbool true; x ] | "==", [ x; ACbool true ] -> Lit x
        | "==", [ a; b ] ->
            let c = Sexplib.Sexp.compare (sexp_of_lit a) (sexp_of_lit b) in
            if c == 0 then Lit (ACbool true)
            else if c < 0 then MethodPred (mp, [ a; b ])
            else MethodPred (mp, [ b; a ])
        | "==", _ -> _failatwith __FILE__ __LINE__ ""
        | "!=", [ ACbool false; x ] | "!=", [ x; ACbool false ] -> Lit x
        | "!=", [ ACbool true; x ] | "!=", [ x; ACbool true ] ->
            aux (Not (Lit x))
        | "!=", [ a; b ] ->
            let c = Sexplib.Sexp.compare (sexp_of_lit a) (sexp_of_lit b) in
            if c == 0 then Lit (ACbool false)
            else if c < 0 then MethodPred (mp, [ a; b ])
            else MethodPred (mp, [ b; a ])
        | "!=", _ -> _failatwith __FILE__ __LINE__ ""
        | _, _ -> MethodPred (mp, args))
    | Implies (e1, e2) -> (
        let e1, e2 = map2 aux (e1, e2) in
        match e1 with
        | Lit (ACbool true) -> e2
        | Lit (ACbool false) -> Lit (ACbool true)
        | _ ->
            if Sexplib.Sexp.compare (sexp_of_t e1) (sexp_of_t e2) == 0 then
              Lit (ACbool true)
            else Implies (e1, e2))
    | Ite (e1, e2, e3) -> (
        let e1, e2, e3 = map3 aux (e1, e2, e3) in
        match e1 with
        | Lit (ACbool true) -> e2
        | Lit (ACbool false) -> e3
        | _ -> Ite (e1, e2, e3))
    | Not e -> (
        let e = aux e in
        match e with
        | Lit (ACbool true) -> Lit (ACbool false)
        | Lit (ACbool false) -> Lit (ACbool true)
        | Not (Not e) -> aux e
        | _ -> Not e)
    | And es ->
        let es = List.map aux es in
        let res =
          List.fold_left
            (fun e1 e2 ->
              match (e1, e2) with
              | Lit (ACbool false), _ | _, Lit (ACbool false) ->
                  Lit (ACbool false)
              | Lit (ACbool true), _ -> e2
              | _, Lit (ACbool true) -> e1
              | And es1, And es2 -> And (merge_sets es1 es2)
              | _, And es2 -> And (merge_sets [ e1 ] es2)
              | And es1, _ -> And (merge_sets es1 [ e2 ])
              | _, _ -> And [ e1; e2 ])
            (Lit (ACbool true)) es
        in
        (* let () = *)
        (*   Pp.printf "@{<bold>PEVAL And:@}\n [%s]\n=%s\n" *)
        (*     (Zzdatatype.Datatype.List.split_by_comma Frontend.pretty_layout es) *)
        (*     (Frontend.pretty_layout res) *)
        (* in *)
        res
    | Or es ->
        List.fold_left
          (fun e1 e2 ->
            match (e1, e2) with
            | Lit (ACbool true), _ | _, Lit (ACbool true) -> Lit (ACbool true)
            | Lit (ACbool false), _ -> e2
            | _, Lit (ACbool false) -> e1
            | Or es1, Or es2 -> Or (merge_sets es1 es2)
            | _, Or es2 -> Or (merge_sets [ e1 ] es2)
            | Or es1, _ -> Or (merge_sets es1 [ e2 ])
            | _, _ -> Or [ e1; e2 ])
          (Lit (ACbool false))
        @@ List.map aux es
    | Iff (e1, e2) ->
        let e1, e2 = map2 aux (e1, e2) in
        let res =
          match (e1, e2) with
          | Lit (ACbool false), e | e, Lit (ACbool false) -> e
          | Lit (ACbool true), e | e, Lit (ACbool true) -> e
          | _, _ ->
              let c = Sexplib.Sexp.compare (sexp_of_t e1) (sexp_of_t e2) in
              if c == 0 then Lit (ACbool true)
              else if c < 0 then Iff (e1, e2)
              else Iff (e2, e1)
        in
        (* let () = *)
        (*   Pp.printf "@{<bold>PEVAL Iff:@}\n [%s]\n=%s\n" *)
        (*     (Zzdatatype.Datatype.List.split_by_comma Frontend.pretty_layout *)
        (*        [ e1; e2 ]) *)
        (*     (Frontend.pretty_layout res) *)
        (* in *)
        res
    | Forall (u, e) ->
        let e' = aux e in
        let res =
          if List.exists (String.equal u.x) @@ fv e' then Forall (u, e') else e'
        in
        (* let () = *)
        (*   Pp.printf "@{<bold>PEVAL Forall:@}\n [%s]\n=%s\n" *)
        (*     (Zzdatatype.Datatype.List.split_by_comma Frontend.pretty_layout *)
        (*        [ e ]) *)
        (*     (Frontend.pretty_layout res) *)
        (* in *)
        res
    | Exists (u, e) ->
        let e = aux e in
        if List.exists (String.equal u.x) @@ fv e then Exists (u, e) else e
  in
  let res = aux prop in
  (* let () = *)
  (*   Pp.printf "@{<bold>PEVAL:@}\n %s\n=%s\n" *)
  (*     (Frontend.pretty_layout prop) *)
  (*     (Frontend.pretty_layout res) *)
  (* in *)
  res
