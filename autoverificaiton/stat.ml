open Ast

let size prop =
  let rec aux size t =
    match t with
    | Lit _ -> size + 1
    | MethodPred (_, _) -> size + 1
    | Implies (e1, e2) -> aux (aux (size + 1) e1) e2
    | Ite (e1, e2, e3) -> aux (aux (aux (size + 2) e1) e2) e3
    | Not e -> aux (size + 1) e
    | And es -> List.fold_left aux size es
    | Or es -> List.fold_left aux size es
    | Iff (e1, e2) -> aux (aux (size + 2) e1) e2
    | Forall (_, e) -> aux (size + 1) e
    | Exists (_, e) -> aux (size + 1) e
  in
  aux 0 prop
