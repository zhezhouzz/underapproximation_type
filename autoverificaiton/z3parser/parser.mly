%{
    open Past
    (* open Grammar *)
    %}
(* tokens *)
%token EOF LPAR RPAR
(* keywords *)
%token EQ NOT AND OR LPARVAR TRUE FALSE
(* %token <string> IDENT *)
%token <string> STRING
%token <int> NUMBER

(* start symbol *)
%start <Past.term Past.loc> prog_eof
%on_error_reduce statements
%%

prog_eof:
  | EOF {{loc = $startpos; x = And []}}
  | s=statement ; EOF { s }
;
statements:
  | s1=statement s2=statements {s1 :: s2}
  | s1=statement {[s1]}
;
statement:
  | LPAR AND s=statements RPAR {{loc = $startpos; x = And s}}
  | LPAR OR s=statements RPAR {{loc = $startpos; x = Or s}}
  | LPAR NOT s=statement RPAR {{loc = $startpos; x = Not s}}
  | LPAR EQ v=var n=NUMBER RPAR {{loc = $startpos; x = OpVarEqInt (v, n)}}
  | TRUE {{loc = $startpos; x = True}}
  | FALSE {{loc = $startpos; x = Not {loc = $startpos; x = True}}}
;
var:
  | LPARVAR n=NUMBER RPAR {n}
  | LPARVAR n=STRING RPAR {int_of_string n}
;
%%
