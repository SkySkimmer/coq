Require Import ssrmatching Ltac2.Ltac2.

Declare ML Module "rocq-runtime.plugins.ltac2_ssr".

Ltac2 Type ssrpat.

Ltac2 @external pattern_T : preterm -> ssrpat := "rocq-runtime.plugins.ltac2_ssr" "pattern_T".

Ltac2 @external pattern_X_In_T : ident -> preterm -> ssrpat :=
  "rocq-runtime.plugins.ltac2_ssr" "pattern_X_In_T".

Ltac2 @external ssrpattern : ssrpat -> unit := "rocq-runtime.plugins.ltac2_ssr" "ssrpattern".


Lemma test (H : True -> True -> 3 = 7) : 28 = 3 * 4.
Proof.
  ssrpattern (pattern_X_In_T @X preterm:(X * 4)).
  match! goal with
    [ |- let selected := 3 in 28 = selected * 4 ] => ()
  end.
Abort.

(* cannot be bothered dealing with actual fresh names *)
Ltac2 Notation "at" "[" pat(tactic) "]" t(tactic) :=
  ssrpattern pat;
  ltac1:(
    let name := ident:(name) in
    intro name;
    pose proof (refl_equal name) as def_name;
    unfold name at 1 in def_name);
  t @def_name;
  ltac1:(
    let name := ident:(name) in
    let def_name := ident:(def_name) in
    try rewrite <- def_name;
    clear name def_name).

(* ltac2 notation "rewrite H in place" uses "place" as a static ident (@place) *)
Ltac2 rewrite_in lem place :=
  let (tac : (unit -> unit) option) := None with (cl : Std.clause option) :=
    Some
      { Std.on_hyps := Some [(place, Std.AllOccurrences, Std.InHyp)]; Std.on_concl :=
            Std.NoOccurrences}
  with (rw : Std.rewriting list) :=
    [{ Std.rew_orient := Some Std.LTR; Std.rew_repeat := Std.Precisely 1; Std.rew_equatn :=
              (fun (_ : unit) => (lem (), Std.NoBindings))}]
  in
  rewrite0 false rw cl tac.

Lemma test'  (H : True -> True -> 3 = 7) : 28 = 3 * 4.
  at [ pattern_X_In_T @X preterm:(X * 4) ] (fun place => rewrite_in (fun () => 'H) place).
  2,3: trivial.
  reflexivity.
Qed.
