(************************************************************************)
(*         *      The Rocq Prover / The Rocq Development Team           *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Names
open Ltac2_plugin
open Ssrmatching_plugin
open Tac2externals
open Tac2ffi
open Ssrmatching

let ltac2_ssr_plugin = "rocq-runtime.plugins.ltac2_ssr"

let pname ?(plugin=ltac2_ssr_plugin) s = { Tac2expr.mltac_plugin = plugin; mltac_tactic = s }

let define ?plugin s = define (pname ?plugin s)

let _return x = Proofview.tclUNIT x

(* XXX no kind? *)
type ssrpat = (Id.t, Ltac_pretype.closed_glob_constr) ssrpattern

let val_ssrpat = Tac2dyn.Val.create "ssrpattern"

let ssrpat : ssrpat repr = repr_ext val_ssrpat

let open_constr_no_classes_flags =
  let open Pretyping in
  {
  use_coercions = true;
  use_typeclasses = Pretyping.NoUseTC;
  solve_unification_constraints = true;
  fail_evar = false;
  expand_evars = false;
  program_mode = false;
  polymorphic = false;
  undeclared_evars_patvars = false;
  patvars_abstract = false;
  unconstrained_sorts = false;
  }

let mkXLetIn x {Ltac_pretype.closure; term} =
  let open Glob_term in
  let x = Name x in
  (* XXX not sure this is correct, it may be better to produce
     "let x := _ in _x" with a closure containing
     "_x := (the original closed term)"? *)
  let term = DAst.make @@ GLetIn (x, None, DAst.make @@ GHole (GBinderType x), None, term) in
  {Ltac_pretype.closure; term}

let interp_funs = {
  mkXLetIn;
  interp_term = Pretyping.understand_uconstr ~flags:open_constr_no_classes_flags;
}

let () = define "ssrpattern" (ssrpat @-> tac unit) @@ fun pat ->
  Proofview.Goal.enter begin fun gl ->
  let open Proofview.Notations in
  let sigma0 = Proofview.Goal.sigma gl in
  let concl0 = Proofview.Goal.concl gl in
  let env = Proofview.Goal.env gl in
  let pat = interp_pattern_gen interp_funs env sigma0 pat in
  let (t, uc), concl_x =
    fill_occ_pattern env sigma0 concl0 pat (Some (false,[])) 1 in
  let sigma = Evd.set_universe_context sigma0 uc in
  let sigma, tty = Typing.type_of env sigma t in
  let concl = EConstr.mkLetIn (Context.make_annot (Name (Id.of_string "selected")) EConstr.ERelevance.relevant, t, tty, concl_x) in
  Proofview.Unsafe.tclEVARS sigma <*>
  Tactics.convert_concl ~cast:false ~check:true concl DEFAULTcast
  end

let () = define "pattern_T" (preterm @-> ret ssrpat) @@ fun t -> T t

let () = define "pattern_X_In_T" (ident @-> preterm @-> ret ssrpat) @@ fun x t -> X_In_T (x,t)
