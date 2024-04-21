From stdpp Require Import mapset.
From CT Require Import Atom.
From CT Require Import CoreLang.
From CT Require Import CoreLangClass.
From CT Require Import BasicTyping.
From CT Require Import BasicTypingProp.
Import CoreLang.

Class AstTyping AST `{Stale aset AST} `{Ast AST} `{AstProp AST} : Type :=
  {
    has_type: context -> AST -> ty -> Prop;
  }.

Notation "Γ '⊢t' t '⋮' T" := (has_type Γ t T) (at level 40).

#[export] Instance tm_asttyping : AstTyping tm :=
  {
    has_type := tm_has_type;
  }.

#[export] Instance value_asttyping : AstTyping value :=
  {
    has_type := value_has_type;
  }.

Class AstTypingProp AST `{Stale aset AST} `{Ast AST} `{AstProp AST} `{AstTyping AST} : Type :=
  {
    basic_typing_contains_fv: forall Γ (e: AST) T, has_type Γ e T -> fv e ⊆ dom Γ;
    basic_typing_closed: forall (e: AST) T, has_type ∅ e T -> closed e;
    basic_typing_regular: forall Γ (e: AST) T, has_type Γ e T -> lc e;
    basic_typing_weaken: forall Γ Γ' (e: AST) T, Γ ⊆ Γ' -> has_type Γ e T -> has_type Γ' e T;
    basic_typing_weaken_insert: forall Γ (e: AST) T x U, x # Γ -> has_type Γ e T -> has_type (<[x := U]>Γ) e T;
    basic_typing_subst: forall Γ z (u: value) U (v: AST) T,
      has_type Γ u U -> has_type (<[z := U]> Γ) v T -> has_type Γ ({z := u} v) T;
    basic_typing_unique: forall Γ (e: AST) T1 T2, Γ ⊢t e ⋮ T1 -> Γ ⊢t e ⋮ T2 -> T1 = T2;
    basic_typing_strengthen: forall Γ (v: AST) T x Tx, (<[x:=Tx]>Γ) ⊢t v ⋮ T -> x # v -> Γ ⊢t v ⋮ T;
  }.


#[export] Instance value_AstTypingProp : AstTypingProp value :=
  {
    basic_typing_contains_fv:= basic_typing_contains_fv_value;
    basic_typing_closed:= basic_typing_closed_value;
    basic_typing_regular:= basic_typing_regular_value;
    basic_typing_weaken:= basic_typing_weaken_value;
    basic_typing_weaken_insert:= basic_typing_weaken_insert_value;
    basic_typing_subst:= basic_typing_subst_value;
    basic_typing_unique:= basic_typing_value_unique;
    basic_typing_strengthen:= basic_typing_strengthen_value;
  }.

#[export] Instance tm_AstTypingProp : AstTypingProp tm :=
  {
    basic_typing_contains_fv:= basic_typing_contains_fv_tm;
    basic_typing_closed:= basic_typing_closed_tm;
    basic_typing_regular:= basic_typing_regular_tm;
    basic_typing_weaken:= basic_typing_weaken_tm;
    basic_typing_weaken_insert:= basic_typing_weaken_insert_tm;
    basic_typing_subst:= basic_typing_subst_tm;
    basic_typing_unique:= basic_typing_tm_unique;
    basic_typing_strengthen:= basic_typing_strengthen_tm;
  }.
