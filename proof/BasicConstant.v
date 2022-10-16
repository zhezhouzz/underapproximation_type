Set Warnings "-notation-overridden,-parsing".

Module BasicConstant.

  Inductive basic_ty : Type :=
  | TNat   : basic_ty
  | TBool  : basic_ty
  | TList  : basic_ty -> basic_ty
  | TTree  : basic_ty -> basic_ty.

  Inductive constant : Type :=
  | cnil: basic_ty -> constant
  | cleaf: basic_ty -> constant
  | ccons: basic_ty -> constant -> constant -> constant
  | cnode: basic_ty -> constant -> constant -> constant -> constant
  | cbool: bool -> constant
  | cnat : nat -> constant.

End BasicConstant.

