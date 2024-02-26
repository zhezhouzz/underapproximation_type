open Sexplib.Std
open Mtyped

type rty =
  | RtyBase of { ou : bool; cty : int }
  | RtyBaseArr of { argrty : rty; arg : (string[@bound]); retty : rty }
  | RtyArrArr of { argrty : rty; retty : rty }
  | RtyTuple of rty list
[@@deriving sexp]

let rec fv_rty (rty_e : rty) =
  match rty_e with
  | RtyBase _ -> []
  | RtyBaseArr { argrty; arg; retty } ->
      Zzdatatype.Datatype.List.substract String.equal
        ([] @ fv_rty retty)
        [ arg ]
      @ fv_rty argrty
  | RtyArrArr { argrty; retty } -> ([] @ fv_rty retty) @ fv_rty argrty
  | RtyTuple rtylist0 -> [] @ List.concat (List.map fv_rty rtylist0)

let rec subst_rty (string_x : string) f (rty_e : rty) =
  match rty_e with
  | RtyBase { ou; cty } -> RtyBase { ou; cty }
  | RtyBaseArr { argrty; arg; retty } ->
      if String.equal arg string_x then
        RtyBaseArr { argrty = subst_rty string_x f argrty; arg; retty }
      else
        RtyBaseArr
          {
            argrty = subst_rty string_x f argrty;
            arg;
            retty = subst_rty string_x f retty;
          }
  | RtyArrArr { argrty; retty } ->
      RtyArrArr
        {
          argrty = subst_rty string_x f argrty;
          retty = subst_rty string_x f retty;
        }
  | RtyTuple rtylist0 -> RtyTuple (List.map (subst_rty string_x f) rtylist0)

let subst_rty_instance x instance e = subst_f_to_instance subst_rty x instance e
(* Generated from _rty.ml *)
