%{
    open Past
    (* open Grammar *)
    %}
(* tokens *)
%token EOF LPAR RPAR LET ITE
(* keywords *)
%token MINUS EQ LT GT LE GE NOT AND OR LPARVAR TRUE FALSE
%token <string> IDENT
%token <string> STRING
%token <int> NUMBER

(* start symbol *)
%start <Past.term Past.loc> prog_eof
%on_error_reduce statements
%%

prog_eof:
  | s=statement ; EOF { s }
;
statements:
  | s1=statement s2=statements {s1 :: s2}
  | s1=statement {[s1]}
;
statement:
  | LPAR ITE s1=statement s2=statement s3=statement RPAR {{loc = $startpos; x = Ite (s1, s2, s3)}}
  | LPAR AND s=statements RPAR {{loc = $startpos; x = And s}}
  | LPAR OR s=statements RPAR {{loc = $startpos; x = Or s}}
  | LPAR NOT s=statement RPAR {{loc = $startpos; x = Not s}}
  | LPAR EQ l1=statement l2=lit RPAR {{loc = $startpos; x = OpEq (l1, l2)}}
  | LPAR LE l1=lit l2=statement RPAR {{loc = $startpos; x = OpLe (l1, l2)}}
  | LPAR LET LPAR LPAR lhs=IDENT rhs=statement RPAR RPAR body=statement RPAR {{loc = $startpos; x = Let (lhs, rhs, body)}}
  | TRUE {{loc = $startpos; x = True}}
  | FALSE {{loc = $startpos; x = Not {loc = $startpos; x = True}}}
  | n=IDENT {{loc = $startpos; x = App (n, [])}}
  | LPAR n=IDENT lits=lits RPAR {{loc = $startpos; x = App (n, lits)}}
  | lit=lit {{loc = $startpos; x = Lit lit}}
;
op:
  | EQ {Eq}
  | LT {Lt}
  | GT {Gt}
  | LE {Le}
  | GE {Ge}
;
lits:
  | s1=lit s2=lits {s1 :: s2}
  | s1=lit {[s1]}
;
lit:
  | LPARVAR n=NUMBER RPAR {VarI n}
  | n=NUMBER {CI n}
  | LPAR MINUS n=NUMBER RPAR {CI (- n)}
;
%%
