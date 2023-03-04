(* open Language *)
open Sugar
open Past
open Zzdatatype.Datatype
open Normalty.Ast.T

let past_lit_to_lit past =
  match past.x with
  | VarI x -> Ast.AVar { ty = Ty_int; x = spf "x%i" x }
  | CI i -> Ast.ACint i

let op_to_string = function
  | Eq -> "=="
  | Lt -> "<"
  | Gt -> ">"
  | Le -> "<="
  | Ge -> ">="

(* let apply past lits = *)
(*   let subst_arg = function CI i -> CI i | VarI i -> List.nth lits i in *)
(*   let rec aux past = *)
(*     match past.x with *)
(*     | True -> past *)
(*     | App (fname, args) -> *)
(*         let args' = List.map subst_arg args in *)
(*         { x = App (fname, args'); loc = past.loc } *)
(*     | Op (op, l1, l2) -> *)
(*         { x = Op (op, subst_arg l1, subst_arg l2); loc = past.loc } *)
(*     | Or l -> { x = Or (List.map aux l); loc = past.loc } *)
(*     | And l -> { x = And (List.map aux l); loc = past.loc } *)
(*     | Not e -> { x = Not (aux e); loc = past.loc } *)
(*     | Let (name, e1, e2) -> { x = Let (name, aux e1, aux e2); loc = past.loc } *)
(*   in *)
(*   aux past *)

(* let past_to_prop (n, past) = *)
(*   let rec aux past = *)
(*     match past.x with *)
(*     | True -> Ast.Lit (ACbool true) *)
(*     | Not { x = True; _ } -> Ast.Lit (ACbool false) *)
(*     | Op (op, x1, x2) -> *)
(*         Ast.( *)
(*           MethodPred *)
(*             (op_to_string op, [ past_lit_to_lit x1; past_lit_to_lit x2 ])) *)
(*     | And es -> Ast.And (List.map aux es) *)
(*     | Or es -> Ast.Or (List.map aux es) *)
(*     | Not e -> Ast.Not (aux e) *)
(*     | Let (fname, e1, e2) -> *)
(*   in *)
(*   let res = aux past in *)
(*   let args = *)
(*     List.init n (fun i -> Normalty.Ast.Ntyped.{ ty = Ty_int; x = spf "x%i" i }) *)
(*   in *)
(*   (args, res) *)

let past_var_space past =
  let space = Hashtbl.create 100 in
  let add i = if Hashtbl.mem space i then () else Hashtbl.add space i () in
  let add_lit = function CI i -> add i | VarI _ -> () in
  let rec aux past =
    match past.x with
    | True -> ()
    | Lit lit -> add_lit lit
    | Not { x = True; _ } -> ()
    | OpEq (e, CI i) ->
        aux e;
        add i
    | OpEq (_, _) -> _failatwith __FILE__ __LINE__ ""
    | OpLe (CI i, e) ->
        aux e;
        add i;
        add (i - 1)
    | OpLe _ -> _failatwith __FILE__ __LINE__ ""
    | And es -> List.iter aux es
    | Or es -> List.iter aux es
    | Not e -> aux e
    | Ite (e1, e2, e3) ->
        aux e1;
        aux e2;
        aux e3
    | Let (_, e1, e2) ->
        aux e1;
        aux e2
    | App (_, lits) -> List.iter add_lit lits
  in
  let () = aux past in
  Hashtbl.to_seq_keys space

let pasts_var_space ps =
  let s =
    List.fold_left
      (fun s p -> IntSet.add_seq (past_var_space p) s)
      IntSet.empty ps
  in
  List.of_seq @@ IntSet.to_seq s

type value = I of int | B of bool

let exec ftab name arr =
  let aux_lit arr = function VarI i -> arr.(i) | CI i -> I i in
  let rec aux ctx arr past =
    match past.x with
    | True -> B true
    | Not { x = True; _ } -> B false
    | Lit lit -> aux_lit arr lit
    | And es ->
        let rec loop = function
          | [] -> B true
          | h :: t -> (
              match aux ctx arr h with
              | B true -> loop t
              | B false -> B false
              | I _ -> _failatwith __FILE__ __LINE__ "")
        in
        loop es
    | Or es ->
        let rec loop = function
          | [] -> B false
          | h :: t -> (
              match aux ctx arr h with
              | B false -> loop t
              | B true -> B true
              | I _ -> _failatwith __FILE__ __LINE__ "")
        in
        loop es
    | Not e -> (
        match aux ctx arr e with
        | B b -> B (not b)
        | I _ -> _failatwith __FILE__ __LINE__ "")
    | OpEq (e, CI i2) -> (
        match aux ctx arr e with
        | I i1 -> B (i1 == i2)
        | B _ -> _failatwith __FILE__ __LINE__ "")
    | OpEq (_, _) -> _failatwith __FILE__ __LINE__ ""
    | OpLe (CI i1, e) -> (
        match aux ctx arr e with
        | I i2 -> B (i1 <= i2)
        | B _ -> _failatwith __FILE__ __LINE__ "")
    | OpLe _ -> _failatwith __FILE__ __LINE__ ""
    | Ite (e1, e2, e3) -> (
        match aux ctx arr e1 with
        | B true -> aux ctx arr e2
        | B false -> aux ctx arr e3
        | I _ -> _failatwith __FILE__ __LINE__ "")
    | Let (name, e1, e2) ->
        let ctx = StrMap.add name (aux ctx arr e1) ctx in
        aux ctx arr e2
    | App (name, lits) -> (
        match lits with
        | [] ->
            StrMap.find
              (spf "autov exec let body: cannot find %s" name)
              ctx name
        | _ ->
            let _, f =
              StrMap.find (spf "autov exec App: cannot find %s" name) ftab name
            in
            aux StrMap.empty (Array.of_list @@ List.map (aux_lit arr) lits) f)
  in
  let _, f = StrMap.find "autov exec App" ftab name in
  aux StrMap.empty arr f

let exec_bool ftab name l =
  let arr = Array.of_list @@ List.map (fun i -> I i) l in
  match exec ftab name arr with
  | B b -> b
  | I _ -> _failatwith __FILE__ __LINE__ ""

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

(* let parse_to_prop (n, str) = *)
(*   let term = parse_string str in *)
(*   past_to_prop (n, term) *)

(* let parse_to_func (n, str) = *)
(*   let term = parse_string str in *)
(*   let arr = past_var_space (n, term) in *)
(*   (Array.to_list arr, past_to_function term) *)
