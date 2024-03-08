open Raw_term
open Term
open Item
open Mtyped

let rec typed_value_to_typed_raw_term (value_e : ('t, 't value) typed) =
  match value_e.x with
  | VConst constant0 -> (Const constant0) #: value_e.ty
  | VVar _t_stringtyped0 -> (Var _t_stringtyped0) #: value_e.ty
  | VLam { lamarg; body } ->
      (Lam { lamarg; lambody = typed_term_to_typed_raw_term body })
      #: value_e.ty
  | VFix { fixarg; body; _ } ->
      (* let tmp = (VLam { lamarg = fixarg; body }) #: body.ty in *)
      let tmp = (VLam { lamarg = fixarg; body }) #: value_e.ty in
      typed_value_to_typed_raw_term tmp
  | VTu _t__tvaluetypedlist0 ->
      (Tu (List.map typed_value_to_typed_raw_term _t__tvaluetypedlist0))
      #: value_e.ty

and typed_term_to_typed_raw_term (term_e : ('t, 't term) typed) =
  match term_e.x with
  | CErr -> Err #: term_e.ty
  | CVal _t__tvaluetyped0 -> typed_value_to_typed_raw_term _t__tvaluetyped0
  | CLetE { rhs; lhs; body } ->
      (Let
         {
           rhs = typed_term_to_typed_raw_term rhs;
           lhs = [ lhs ];
           letbody = typed_term_to_typed_raw_term body;
           if_rec = false;
         })
      #: term_e.ty
  | CLetDeTu { turhs; tulhs; body } ->
      (Let
         {
           rhs = typed_value_to_typed_raw_term turhs;
           lhs = tulhs;
           letbody = typed_term_to_typed_raw_term body;
           if_rec = false;
         })
      #: term_e.ty
  | CApp { appf; apparg } ->
      (App
         ( typed_value_to_typed_raw_term appf,
           [ typed_value_to_typed_raw_term apparg ] ))
      #: term_e.ty
  | CAppOp { op; appopargs } ->
      (AppOp (op, List.map typed_value_to_typed_raw_term appopargs))
      #: term_e.ty
  | CMatch { matched; match_cases } ->
      (Match
         {
           matched = typed_value_to_typed_raw_term matched;
           match_cases = List.map macth_case_to_raw_macth_case match_cases;
         })
      #: term_e.ty

and macth_case_to_raw_macth_case = function
  | CMatchcase { constructor; args; exp } ->
      Matchcase { constructor; args; exp = typed_term_to_typed_raw_term exp }

let denormalize_term = typed_term_to_typed_raw_term
let denormalize_value = typed_value_to_typed_raw_term

let denormalize_item (item : Nt.t item) =
  match item with
  | MFuncImp { name; if_rec; body } ->
      let body = denormalize_term body in
      MFuncImpRaw { name; if_rec; body }
  | _ -> item

let denormalize_structure = List.map denormalize_item
