(************************************************************************)
(*         *      The Rocq Prover / The Rocq Development Team           *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Util
open Goptions
open Vernacexpr

let vernac_set_option ~locality ~stage key opt =
  match opt with
  | OptionUnset -> unset_option_value_gen ~locality ~stage key
  | OptionSetString s -> set_string_option_value_gen ~locality ~stage key s
  | OptionSetInt n -> set_int_option_value_gen ~locality ~stage key (Some n)
  | OptionSetTrue -> set_bool_option_value_gen ~locality ~stage key true

let vernac_set_option ~locality ~stage table v =
  let () =
    if String.equal "Append" (List.last table) then
      CErrors.user_err Pp.(str "Set ... Append is not supported.");
  in
  vernac_set_option ~locality ~stage table v

let iter_table f k v = Goptions.iter_table (Global.env()) f k v

let vernac_add_option local = iter_table { aux = fun table env x -> table.add env local x }

let vernac_remove_option local = iter_table { aux = fun table env x -> table.remove env local x }

let vernac_mem_option = iter_table { aux = fun table -> table.mem }

let vernac_print_option key =
  try (get_ref_table key).print ()
  with Not_found ->
  try (get_string_table key).print ()
  with Not_found ->
  try print_option_value key
  with Not_found -> error_undeclared_key key
