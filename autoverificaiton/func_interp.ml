open Z3
open Sugar

type result = { ifcases : (Expr.expr * bool) list; elsecase : Expr.expr * bool }

open Zzdatatype.Datatype
module NT = Normalty.Ast.T
module Ntyped = Normalty.Ast.Ntyped

let get_preds_interp _ model =
  let funcs = Model.get_func_decls model in
  let get func =
    (* let () = Printf.printf ">> %s\n" (FuncDecl.to_string func) in *)
    match Model.get_func_interp model func with
    | None -> _failatwith __FILE__ __LINE__ "never happen"
    | Some interp ->
        let func_name = Symbol.to_string @@ FuncDecl.get_name func in
        let arity = Model.FuncInterp.get_arity interp in
        let handle e =
          let open Model.FuncInterp.FuncEntry in
          let b = Z3aux.z3expr_to_bool (get_value e) in
          let args = get_args e in
          let argsname = List.map Expr.to_string args in
          let () =
            Printf.printf "(%s) --> %b\n" (StrList.to_string argsname) b
          in
          ()
        in
        let entries = List.map handle @@ Model.FuncInterp.get_entries interp in
        let _ =
          if List.length entries > 0 then _failatwith __FILE__ __LINE__ ""
          else ()
        in
        let e = Model.FuncInterp.get_else interp in
        (* let args, prope = Parse.parse_to_prop (arity, Expr.to_string e) in *)
        (* let () = *)
        (*   Printf.printf "get_entries[%s] (%i)\n" func_name *)
        (*     (List.length (Model.FuncInterp.get_entries interp)) *)
        (* in *)
        (* let () = *)
        (*   Printf.printf "else[%s]: %s\n" *)
        (*     (List.split_by_comma (fun x -> x.Ntyped.x) args) *)
        (*     (Frontend.pretty_layout prope) *)
        (* in *)
        let space, f = Parse.parse_to_func (arity, Expr.to_string e) in
        let () =
          List.iter (fun l ->
              Printf.printf "%s(%s) := %b\n" func_name
                (List.split_by_comma string_of_int l)
                (f (Array.of_list l)))
          @@ List.choose_list_list space
        in
        (func_name, arity, space, f)
  in
  List.map get funcs
