From stdpp Require Import mapset.
From stdpp Require Import natmap.
From stdpp Require Import fin_map_dom.
From CT Require Import CoreLangProp.
From CT Require Import OperationalSemanticsProp.
From CT Require Import BasicTypingProp.
From CT Require Import SyntaxSugar.
From CT Require Import Refinement.
From CT Require Import RefinementTac.
From CT Require Import RefinementDenotation.
From CT Require Import RefinementDenotationTac.
From CT Require Import RefinementDenotationProp.
From CT Require Import RDInv.
From CT Require Import RDInv2.
From CT Require Import RDInv3.
From CT Require Import TermOrdering.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Classical.

Import Atom.
Import CoreLang.
Import CoreLangProp.
Import Tactics.
Import NamelessTactics.
Import ListCtx.
Import OperationalSemantics.
Import OperationalSemanticsProp.
Import BasicTyping.
Import SyntaxSugar.
Import Refinement.
Import RefinementTac.
Import RefinementDenotation.
Import RefinementDenotationTac.
Import RefinementDenotationProp.
Import WFDenotation.
Import WFDenotationTac.
Import NamelessTactics.
Import TermOrdering.
Import RDInv.
Import RDInv2.
Import RDInv3.

Global Hint Resolve mk_eq_constant_is_not_overbasety: core.
Global Hint Resolve mk_eq_var_is_not_overbasety: core.
Global Hint Resolve is_arr_implies_not_overbasety: core.
Global Hint Constructors ok_dctx: core.
Global Hint Resolve rR_implies_no_free: core.
Global Hint Resolve ctxrR_tlete_drop_halt_lhs: core.
Global Hint Resolve rR_implies_reduction_no_free: core.
Global Hint Resolve is_arr_implies_not_overbasety: core.
Global Hint Resolve under_not_overbasety: core.
