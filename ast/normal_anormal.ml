module T = struct
  open Sexplib.Std

  type ty = Normalty.T.t [@@deriving sexp]
  type id = Strid.T.t [@@deriving sexp]
  type 'a typed = { ty : ty option; x : 'a } [@@deriving sexp]

  type term =
    | Const of Value.t
    | Var of id
    | Tu of id typed list
    | Lam of id typed list * term typed
    | Fix of id typed * term typed
    | App of id typed * id typed list
    | Let of id typed list * term typed * term typed
    | Ite of id typed * term typed * term typed
    | Match of id typed * case list

  and case = { constuctor : id; args : id list; exp : term typed }
  [@@deriving sexp]

  let make_untyped x = { ty = None; x }
  let make_untyped_var id = { ty = None; x = Var id }

  (* let make_untyped_tuple ids = *)
  (*   { ty = None; x = Tu (List.map make_untyped_var ids) } *)

  (* let make_untyped_id_app (id, ids) = *)
  (*   { ty = None; x = App (make_untyped_var id, List.map make_untyped_var ids) } *)
end
