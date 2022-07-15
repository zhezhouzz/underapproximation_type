type ty

type id

type 'a typed = {ty: ty option; x: 'a}

type term =
  | Const of Value.t
  | Var of id
  | Tu of term typed list
  | Lam of id typed list * term typed
  | Fix of id typed * term typed
  | App of term typed * term typed list
  | Let of id typed list * term typed * term typed
  | Ite of term typed * term typed * term typed
  | Match of id typed * case list
and case = {constuctor : id;
            args : id list;
            exp : term typed}
