open Z3
open Sugar

type result = { ifcases : (Expr.expr * bool) list; elsecase : Expr.expr * bool }

open Zzdatatype.Datatype
module NT = Normalty.Ast.T

let get_preds_interp _ model =
  let funcs = Model.get_func_decls model in
  let get func =
    let () = Printf.printf ">> %s\n" (FuncDecl.to_string func) in
    match Model.get_func_interp model func with
    | None -> _failatwith __FILE__ __LINE__ "never happen"
    | Some interp ->
        let func_name = Symbol.to_string @@ FuncDecl.get_name func in
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
        let _ = List.map handle (Model.FuncInterp.get_entries interp) in
        let e = Model.FuncInterp.get_else interp in
        let num_args = Expr.get_num_args e in
        let eargs = Expr.get_args e in
        let () =
          Printf.printf "get_entries[%s] (%i)\n" func_name
            (List.length (Model.FuncInterp.get_entries interp))
        in
        let () =
          Printf.printf "get_entries[%s(%i)]: (%s) %s\n" func_name num_args
            (List.split_by_comma Expr.to_string eargs)
            (Expr.to_string e)
        in
        let _ =
          Printf.printf "SEXP:\n%s\n" @@ AST.to_sexpr @@ Expr.ast_of_expr e
        in
        (* let () = Printf.printf ">> %s\n" (Model.FuncInterp.to_string interp) in *)
        (* let e' = *)
        (*   Expr.substitute_vars e [ Z3aux.tpedvar_to_z3 ctx (NT.Ty_int, "a") ] *)
        (* in *)
        (* let () = Printf.printf ">> %s\n" (Expr.to_string e') in *)
        ()
  in
  let () = Printf.printf "get_preds_interp (%i)\n" (List.length funcs) in
  List.map get funcs
