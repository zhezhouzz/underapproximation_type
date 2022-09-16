open Abt
open Format

let op_tostring op =
  let aux op =
    match op with
    | Onum i -> sprintf "num[%i]" i
    | Ostr s -> sprintf "str[%s]" s
    | Oplus -> "plus"
    | Otimes -> "times"
    | Ocat -> "times"
    | Olen -> "len"
    | Olet -> "let"
  in
  aux op

let vs_tostring vs =
  match vs with
  | [] -> None
  | [v] -> Some v
  | _ ->
    Some (
      sprintf "(%s)" @@
      List.fold_left (fun s v ->
          sprintf "%s, %s" s v
        ) "" vs)


let rec pprint (indent: int) abt : unit =
  let make_bindings l =
    List.iter (fun (vs, abt') ->
        (print_break 0 indent;
         match vs_tostring vs with
         | None -> printf "@["
         | Some s -> printf "@[%s." s);
        pprint (indent + 2) abt';
        printf ";@]@."
      ) l in
  match abt with
  | Aleaf x -> printf "@[%s@]" x
  | Anode (op, bindings) ->
    match bindings with
    | [] -> printf "@[%s@]" (op_tostring op)
    | _ ->
      printf "@[%s@](@;" (op_tostring op);
      make_bindings bindings;
      print_break 0 indent;
      printf ")@]"

let print_abt abt = (pprint 2 abt); printf "\n"
