(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

type t

val of_constr : Constr.t -> t

val uid : t -> int
(** unique identifier for the physical constr (unique over a given of_constr call) *)

val refcount : t -> int
(** How many times it appears (from the original to_constr call, count by graph size not tree size) *)

val self : t -> Constr.t

val kind : t -> (t, t, Sorts.t, UVars.Instance.t, Sorts.relevance) Constr.kind_of_term
