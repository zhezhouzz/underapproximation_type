open Mtyped
module Nt = Normalty.Frontend
open Sugar
open Zzdatatype.Datatype

let layout_typectx (layout : 'a -> string) ctx : string =
  match ctx with
  | Typectx.Typectx l ->
      List.split_by_comma (fun { x; ty } -> spf "%s:%s" x (layout ty)) l

let pprint_typectx f ctx =
  Env.show_debug_typing (fun _ ->
      match ctx with
      | Typectx.Typectx ctx ->
          if List.length ctx == 0 then Pp.printf "@{<green>∅@}"
          else
            List.iter
              (fun { x; ty } ->
                Pp.printf "%s:@{<green>%s@}," x (f ty)
                (* (List.split_by "∧" f ty) *))
              ctx)

let playout_judge ctx (e, ty) = Printf.sprintf "%s⊢\n%s :\n%s\n" ctx e ty

(* let playout_under_judge ctx (e, (r : Languages.Qunderty.t)) = *)
(*   playout_judge (playout_under ctx) (e, Qunderty.layout_typectx r) *)

let playout_subtyping ctx (r1, r2) = Printf.sprintf "%s⊢\n%s <:\n%s\n" ctx r1 r2
