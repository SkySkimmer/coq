(************************************************************************)
(*         *      The Rocq Prover / The Rocq Development Team           *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

val fix_double_click :
  < buffer : < get_iter : [> `INSERT ] -> GText.iter;
               move_mark : [> `INSERT | `SEL_BOUND ] ->
                 where:GText.iter -> unit;
               .. >;
    event : < connect :
                < button_press :
                    callback:([> `TWO_BUTTON_PRESS ] Gdk.event ->
                              bool) ->
                    'a;
                  .. >;
              .. >;
    .. > ->
  unit
val starts_word : GText.iter -> bool
val ends_word : GText.iter -> bool
val find_word_start : GText.iter -> GText.iter
val find_word_end : GText.iter -> GText.iter
