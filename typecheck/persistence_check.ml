open Languages
open Autov.Prop
open Underctx
open Sugar

(* TODO: a real persistence check *)
let persistence_check uctx ty =
  match UT.assume_base_destruct_opt ty with
  | None -> true
  | Some (nu, _, prop) -> (
      match prop with
      | MethodPred ("==", [ AVar v; AVar x ]) -> (
          let qv =
            if String.equal v.x nu then Some x
            else if String.equal x.x nu then Some v
            else None
          in
          match qv with
          | None -> false
          | Some qv -> (
              match MustMayTypectx.get_ty uctx.ctx qv.x with
              | MMT.Ot _ -> true
              | MMT.Ut _ -> false
              | MMT.Consumed _ -> false
              | MMT.NoRefinement _ -> _failatwith __FILE__ __LINE__ qv.x))
      | _ -> false)
