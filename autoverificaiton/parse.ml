(* open Language *)
open Sugar
open Past
open Zzdatatype.Datatype
open Normalty.Ast.T

let past_to_prop (n, past) =
  let rec aux past =
    match past.x with
    | True -> Ast.Lit (ACbool true)
    | Not { x = True; _ } -> Ast.Lit (ACbool false)
    | OpVarEqInt (x, i) ->
        Ast.(
          MethodPred ("==", [ AVar { ty = Ty_int; x = spf "x%i" x }; ACint i ]))
    | And es -> Ast.And (List.map aux es)
    | Or es -> Ast.Or (List.map aux es)
    | Not e -> Ast.Not (aux e)
  in
  let res = aux past in
  let args =
    List.init n (fun i -> Normalty.Ast.Ntyped.{ ty = Ty_int; x = spf "x%i" i })
  in
  (args, res)

let past_var_space (n, past) =
  let arr = Array.init n (fun _ -> []) in
  let rec aux past =
    match past.x with
    | True -> ()
    | Not { x = True; _ } -> ()
    | OpVarEqInt (idx, i) -> arr.(idx) <- i :: arr.(idx)
    | And es -> List.iter aux es
    | Or es -> List.iter aux es
    | Not e -> aux e
  in
  let () = aux past in
  let arr =
    Array.map
      (fun l ->
        let l = List.slow_rm_dup ( == ) l in
        match IntList.max_opt l with Some n -> (n + 1) :: l | None -> [ 0 ])
      arr
  in
  arr

let past_to_function past =
  let rec aux past =
    match past.x with
    | True -> fun _ -> true
    | Not { x = True; _ } -> fun _ -> false
    | OpVarEqInt (idx, i) -> fun arr -> arr.(idx) == i
    | And es ->
        fun arr -> List.fold_left (fun res e -> res && aux e arr) true es
    | Or es ->
        fun arr -> List.fold_left (fun res e -> res || aux e arr) false es
    | Not e -> fun arr -> not (aux e arr)
  in
  aux past

let past_to_function_0 past =
  let rec aux past =
    match past.x with
    | True -> true
    | Not { x = True; _ } -> false
    | OpVarEqInt (_, _) -> _failatwith __FILE__ __LINE__ ""
    | And es -> List.fold_left (fun res e -> res && aux e) true es
    | Or es -> List.fold_left (fun res e -> res || aux e) false es
    | Not e -> not (aux e)
  in
  aux past

let past_to_function_1 past =
  let rec aux past =
    match past.x with
    | True -> fun _ -> true
    | Not { x = True; _ } -> fun _ -> false
    | OpVarEqInt (0, i) -> fun x -> x == i
    | OpVarEqInt (_, _) -> _failatwith __FILE__ __LINE__ ""
    | And es -> fun x -> List.fold_left (fun res e -> res && aux e x) true es
    | Or es -> fun x -> List.fold_left (fun res e -> res || aux e x) false es
    | Not e -> fun x -> not (aux e x)
  in
  aux past

let past_to_function_2 past =
  let rec aux past =
    match past.x with
    | True -> fun _ _ -> true
    | Not { x = True; _ } -> fun _ _ -> false
    | OpVarEqInt (0, i) -> fun x _ -> x == i
    | OpVarEqInt (1, i) -> fun _ x -> x == i
    | OpVarEqInt (_, _) -> _failatwith __FILE__ __LINE__ ""
    | And es ->
        fun x y -> List.fold_left (fun res e -> res && aux e x y) true es
    | Or es ->
        fun x y -> List.fold_left (fun res e -> res || aux e x y) false es
    | Not e -> fun x y -> not (aux e x y)
  in
  aux past

let past_to_function_3 past =
  let rec aux past =
    match past.x with
    | True -> fun _ _ _ -> true
    | Not { x = True; _ } -> fun _ _ _ -> false
    | OpVarEqInt (0, i) -> fun x _ _ -> x == i
    | OpVarEqInt (1, i) -> fun _ x _ -> x == i
    | OpVarEqInt (2, i) -> fun _ _ x -> x == i
    | OpVarEqInt (_, _) -> _failatwith __FILE__ __LINE__ ""
    | And es ->
        fun x y z -> List.fold_left (fun res e -> res && aux e x y z) true es
    | Or es ->
        fun x y z -> List.fold_left (fun res e -> res || aux e x y z) false es
    | Not e -> fun x y z -> not (aux e x y z)
  in
  aux past

let layout_position (p : Lexing.position) =
  let open Lexing in
  spf "At line %i, offset %i: syntax error" p.pos_lnum (p.pos_cnum - p.pos_bol)

let parse_ linebuf =
  try Parser.prog_eof Lexer.next_token linebuf with
  | Lexer.LexError msg -> raise @@ failwith (Printf.sprintf "%s%!" msg)
  | Parser.Error ->
      raise @@ failwith (layout_position @@ Lexing.lexeme_end_p linebuf)

let parse filename =
  (* let _ = Printf.printf "parsing: %s\n" filename in *)
  let oc = open_in filename in
  let linebuf = Lexing.from_channel oc in
  let res = parse_ linebuf in
  close_in oc;
  res

let parse_string str = parse_ @@ Lexing.from_string str

let parse_to_prop (n, str) =
  let term = parse_string str in
  past_to_prop (n, term)

let parse_to_func (n, str) =
  let term = parse_string str in
  let arr = past_var_space (n, term) in
  (Array.to_list arr, past_to_function term)
