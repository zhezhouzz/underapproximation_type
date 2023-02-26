(* open Language *)
open Sugar
open Past
open Zzdatatype.Datatype

let rec layout_lit t =
  let rec aux t =
    match t.x with
    | Str s -> spf "\"%s\"" s
    | Var s -> s
    | List lits -> spf "[%s]" @@ List.split_by_comma aux lits
    | Unit -> "()"
    | Block sts -> spf "{\n%s\n}" @@ List.split_by "\n" layout_statement sts
    | App a -> spf "(%s)" @@ layout_application a
  in
  aux t

and layout_application t =
  let f, args = t.x in
  spf "%s %s" (layout_lit f) @@ List.split_by " " layout_lit args

and layout_statement t =
  let aux t =
    match t with
    | Include s -> spf "include \"%s\";" s
    | Assign (None, a) -> spf "%s;" @@ layout_application a
    | Assign (Some name, a) -> spf "%s <- %s;" name @@ layout_application a
  in
  aux t.x

let layout_prog sts = List.split_by "\n" layout_statement sts

let layout_position (p : Lexing.position) =
  let open Lexing in
  spf "At line %i, offset %i: syntax error" p.pos_lnum (p.pos_cnum - p.pos_bol)

let parse_ linebuf =
  try Parser.prog_eof Lexer.next_token linebuf with
  | Lexer.LexError msg -> raise @@ failwith (Printf.sprintf "%s%!" msg)
  | Parser.Error ->
      raise @@ failwith (layout_position @@ Lexing.lexeme_end_p linebuf)

let parse filename =
  let oc = open_in filename in
  let linebuf = Lexing.from_channel oc in
  let res = parse_ linebuf in
  close_in oc;
  res

let parse_string str = parse_ @@ Lexing.from_string str

type parsing_res = ParseDefault | ParsePre of string | ParseF of string

(* TODO: multiple line i_err *)
let i_err_name = "i_err"

let extract_saw_script t =
  match List.rev t with
  | {
      x =
        Assign
          ( None,
            {
              x =
                ( { x = Var "prove_extcore"; _ },
                  [
                    {
                      x =
                        App
                          {
                            x = { x = Var "do"; _ }, [ { x = Block sts; _ } ];
                            _;
                          };
                      _;
                    };
                    goal_name;
                  ] );
              _;
            } );
      _;
    }
    :: rest ->
      (List.rev rest, sts, goal_name)
  | _ -> failwith "wrong format"

let construct_saw_script header sts goal_name =
  let loc = Lexing.dummy_pos in
  header
  @ [
      {
        x =
          Assign
            ( None,
              {
                x =
                  ( { x = Var "prove_extcore"; loc },
                    [
                      {
                        x =
                          App
                            {
                              x =
                                ( { x = Var "do"; loc },
                                  [ { x = Block sts; loc } ] );
                              loc;
                            };
                        loc;
                      };
                      goal_name;
                    ] );
                loc;
              } );
        loc;
      };
    ]

open Language.Ast

let to_rewrite_rules args =
  let args =
    List.map
      (fun x -> match x.x with Var name -> name | _ -> failwith "die")
      args
  in
  args

let to_unint_names args =
  let args =
    List.map
      (fun x -> match x.x with Str name -> name | _ -> failwith "die")
      args
  in
  args

let to_rewrite_rules_set = function
  | Var "empty_ss" -> Empty_ss
  | Var "basic_ss" -> Basic_ss
  | App { x = { x = Var "cryptol_ss"; _ }, [ { x = Unit; _ } ]; _ } ->
      Cryptol_ss
  | lit ->
      failwith
        (spf "to_rewrite_rules_set die (%s)"
        @@ layout_lit { x = lit; loc = Lexing.dummy_pos })

let to_script t =
  let header, sts, goal_name = extract_saw_script t in
  let one t =
    match t.x with
    | Assign (None, { x = f, args; _ }) -> (
        let args = List.map (fun x -> x.x) args in
        match (f.x, args) with
        (* | Var "simplify", [ List l; s ] -> *)
        (*     let s = to_rewrite_rules_set s in *)
        (*     let rules = *)
        (*       List.map (fun x -> Simplify (Addsimps (x, Empty_ss))) *)
        (*       @@ to_rewrite_rules l *)
        (*     in *)
        (*     (rules @ [ Simplify (RuleSet s) ], None) *)
        | Var "simplify", [ App { x = { x = Var "addsimps"; _ }, args; _ } ]
          -> (
            let args = List.map (fun x -> x.x) args in
            match args with
            | [ List l; s ] -> (
                let s = to_rewrite_rules_set s in
                let rules =
                  List.map (fun x -> Simplify (Addsimps (x, Empty_ss)))
                  @@ to_rewrite_rules l
                in
                match s with
                | Empty_ss -> (rules, None)
                | _ -> (rules @ [ Simplify (RuleSet s) ], None))
            | _ -> failwith "die")
        | Var "simplify", [ s ] ->
            let s = to_rewrite_rules_set s in
            ([ Simplify (RuleSet s) ], None)
        | Var "goal_eval_unint", [ List l ] ->
            let names = to_unint_names l in
            ([ Goal_eval_unint names ], None)
        | Var "w4_unint_z3", [ List l ] ->
            let names = to_unint_names l in
            ([], Some (W4_unint_z3 names))
        | Var "w4_unint_yices", [ List l ] ->
            let names = to_unint_names l in
            ([], Some (W4_unint_yices names))
        | Var "unfolding", [ List l ] ->
            let names = to_unint_names l in
            (List.map (fun name -> Unfold name) names, None)
        | Var name, _ -> failwith @@ spf "Parse error at %s" name
        | _ -> failwith "die")
    | _ -> failwith "die"
  in
  let rec aux cs sts =
    match sts with
    | [] -> failwith "die"
    | [ s ] -> (
        match one s with
        | cs', Some solver -> (cs @ cs', solver)
        | _, None -> failwith "die")
    | h :: t -> (
        match one h with
        | cs', None -> aux (cs @ cs') t
        | _, Some _ -> failwith "die")
  in
  (header, aux [] sts, goal_name)

let of_script header (ts, solver) goal_name =
  let mk_loc x = { x; loc = Lexing.dummy_pos } in
  let mk_app_raw f args = mk_loc @@ (mk_loc f, List.map mk_loc args) in
  let mk_app f args = App (mk_app_raw f args) in
  let mk_assign_none f args = mk_loc @@ Assign (None, mk_app_raw f args) in
  let mk_list xs = List (List.map mk_loc xs) in
  let layout_rule_set_lit t =
    let t =
      match t with
      | Empty_ss -> Var "empty_ss"
      | Basic_ss -> Var "basic_ss"
      | Cryptol_ss -> mk_app (Var "cryptol_ss") [ Unit ]
    in
    t
  in
  let layout_rule_set = function
    | RuleSet s -> [ layout_rule_set_lit s ]
    | Addsimps (name, s) ->
        [
          mk_app (Var "addsimps")
            [ mk_list [ Var name ]; layout_rule_set_lit s ];
        ]
  in
  let layout_tactics = function
    | Unfold name -> mk_assign_none (Var "unfolding") [ mk_list [ Str name ] ]
    | Simplify s -> mk_assign_none (Var "simplify") (layout_rule_set s)
    | Goal_eval_unint name ->
        mk_assign_none (Var "goal_eval_unint")
          [ mk_list (List.map (fun x -> Str x) name) ]
  in
  let layout_solver = function
    | Trivial -> mk_assign_none (Var "trivial") []
    | W4_unint_yices names ->
        mk_assign_none (Var "w4_unint_yices")
          [ mk_list @@ List.map (fun x -> Str x) names ]
    | W4_unint_z3 names ->
        mk_assign_none (Var "w4_unint_z3")
          [ mk_list @@ List.map (fun x -> Str x) names ]
  in
  let sts = List.map layout_tactics ts in
  construct_saw_script header (sts @ [ layout_solver solver ]) goal_name

let modify_include header path goalname =
  let loc = Lexing.dummy_pos in
  match header with
  | { x = Include _; _ }
    :: { x = Assign (goal, { x = f, [ { x = Str _; _ } ]; _ }); _ }
    :: rest ->
      { x = Include path; loc }
      :: {
           x = Assign (goal, { x = (f, [ { x = Str goalname; loc } ]); loc });
           loc;
         }
      :: rest
  | _ -> failwith "die"
