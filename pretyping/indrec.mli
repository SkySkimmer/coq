(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Names
open Constr
open Environ
open Evd

(** Errors related to recursors building *)

type recursion_scheme_error =
  | NotAllowedCaseAnalysis of (*isrec:*) bool * Sorts.t * pinductive
  | NotMutualInScheme of inductive * inductive
  | NotAllowedDependentAnalysis of (*isrec:*) bool * inductive

type _ CErrors.tag += RecursionSchemeError : (env * recursion_scheme_error) CErrors.tag

(** Eliminations *)

type dep_flag = bool

(** Build a case analysis elimination scheme in some sort family *)

type case_analysis = private {
  case_params : EConstr.rel_context;
  case_pred : Name.t Context.binder_annot * EConstr.types;
  case_branches : EConstr.rel_context;
  case_arity : EConstr.rel_context;
  case_body : EConstr.t;
  case_type : EConstr.t;
}

val eval_case_analysis : case_analysis -> EConstr.t * EConstr.types

val build_case_analysis_scheme : env -> Evd.evar_map -> pinductive ->
      dep_flag -> Sorts.family -> evar_map * case_analysis

(** Build a dependent case elimination predicate unless type is in Prop
   or is a recursive record with primitive projections. *)

val build_case_analysis_scheme_default : env -> evar_map -> pinductive ->
      Sorts.family -> evar_map * case_analysis

(** Builds a recursive induction scheme (Peano-induction style) in the same
   sort family as the inductive family; it is dependent if not in Prop
   or a recursive record with primitive projections.  *)

val build_induction_scheme : env -> evar_map -> pinductive ->
      dep_flag -> Sorts.family -> evar_map * constr

(** Builds mutual (recursive) induction schemes *)

val build_mutual_induction_scheme :
  env -> evar_map -> ?force_mutual:bool ->
  (pinductive * dep_flag * Sorts.family) list -> evar_map * constr list

(** Scheme combinators *)

(** [weaken_sort_scheme env sigma eq s n c t] derives by subtyping from [c:t]
   whose conclusion is quantified on [Type i] at position [n] of [t] a
   scheme quantified on sort [s]. [set] asks for [s] be declared equal to [i],
  otherwise just less or equal to [i]. *)

val weaken_sort_scheme : env -> evar_map -> bool -> EConstr.ESorts.t -> int -> constr -> types ->
  evar_map * types * constr

(** Recursor names utilities *)

val lookup_eliminator : env -> inductive -> Sorts.family -> GlobRef.t
val elimination_suffix : Sorts.family -> string
val make_elimination_ident : Id.t -> Sorts.family -> Id.t

val case_suffix : string
