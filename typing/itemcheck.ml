open Languagez

let item_check ctx = function
  | =
    List.fold_left
      (fun ctx (x, ty) -> Nctx.add_to_right ctx (x, ty))
      Nctx.empty libs
  in
  List.mapi
    (fun id (_, (name', ty)) ->
      let id = id + 1 in
      let () =
        Env.show_debug_typing @@ fun _ -> Pp.printf "@{<bold>Task %i:@}\n" id
      in
      match List.find_opt (fun { name; _ } -> String.equal name name') l with
      | None ->
          _failatwith __FILE__ __LINE__
            (spf "The source code of given refinement type '%s' is missing."
               name')
      | Some { body; _ } ->
          let () =
            Env.show_debug_typing @@ fun _ ->
            Pp.printf "@{<bold>check against with:@} %s\n" (pretty_layout ty)
          in
          let ctx = empty in
          let res =
            Undersub.type_err_to_false (fun () ->
                type_check { nctx; ctx; libctx } body ty)
          in
          let () =
            if res then
              Env.show_debug_typing @@ fun _ ->
              Pp.printf "@{<bold>@{<yellow>Task %i, type check succeeded@}@}\n"
                id
            else
              Env.show_debug_typing @@ fun _ ->
              Pp.printf "@{<bold>@{<red>Task %i, type check failed@}@}\n" id
          in
          res)
    r


let struc_check l notations libs r =
  let nctx =
    List.fold_left
      (fun ctx (name, ty) -> add_to_right ctx (name, ty))
      empty notations
  in
  let libctx =
    List.fold_left
      (fun ctx (x, ty) -> Nctx.add_to_right ctx (x, ty))
      Nctx.empty libs
  in
  List.mapi
    (fun id (_, (name', ty)) ->
      let id = id + 1 in
      let () =
        Env.show_debug_typing @@ fun _ -> Pp.printf "@{<bold>Task %i:@}\n" id
      in
      match List.find_opt (fun { name; _ } -> String.equal name name') l with
      | None ->
          _failatwith __FILE__ __LINE__
            (spf "The source code of given refinement type '%s' is missing."
               name')
      | Some { body; _ } ->
          let () =
            Env.show_debug_typing @@ fun _ ->
            Pp.printf "@{<bold>check against with:@} %s\n" (pretty_layout ty)
          in
          let ctx = empty in
          let res =
            Undersub.type_err_to_false (fun () ->
                type_check { nctx; ctx; libctx } body ty)
          in
          let () =
            if res then
              Env.show_debug_typing @@ fun _ ->
              Pp.printf "@{<bold>@{<yellow>Task %i, type check succeeded@}@}\n"
                id
            else
              Env.show_debug_typing @@ fun _ ->
              Pp.printf "@{<bold>@{<red>Task %i, type check failed@}@}\n" id
          in
          res)
    r
