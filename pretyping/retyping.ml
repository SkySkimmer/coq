(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

open Pp
open CErrors
open Util
open Term
open Vars
open Preinductive
open Inductive
open Inductiveops
open Names
open Reductionops
open Environ
open Termops
open Arguments_renaming
open Context.Rel.Declaration

module RelDecl = Context.Rel.Declaration
module NamedDecl = Context.Named.Declaration

type retype_error =
  | NotASort
  | NotAnArity
  | NotAType
  | BadVariable of Id.t
  | BadMeta of int
  | BadRecursiveType
  | NonFunctionalConstruction

let print_retype_error = function
  | NotASort -> str "Not a sort"
  | NotAnArity -> str "Not an arity"
  | NotAType -> str "Not a type (1)"
  | BadVariable id -> str "variable " ++ Id.print id ++ str " unbound"
  | BadMeta n -> str "unknown meta " ++ int n
  | BadRecursiveType -> str "Bad recursive type"
  | NonFunctionalConstruction -> str "Non-functional construction"

exception RetypeError of retype_error

let retype_error re = raise (RetypeError re)

let anomaly_on_error f x =
 try f x
 with RetypeError e -> anomaly ~label:"retyping" (print_retype_error e)

let get_type_from_constraints env sigma t =
  if isEvar (fst (decompose_app t)) then
    match
      List.map_filter (fun (pbty,env,t1,t2) ->
        if is_fconv Reduction.CONV env sigma t t1 then Some t2
        else if is_fconv Reduction.CONV env sigma t t2 then Some t1
        else None)
        (snd (Evd.extract_all_conv_pbs sigma))
    with
    | t::l -> t
    | _ -> raise Not_found
  else raise Not_found

let rec subst_type env sigma typ = function
  | [] -> typ
  | h::rest ->
      match kind_of_term (whd_all env sigma typ) with
        | Prod (na,c1,c2) -> subst_type env sigma (subst1 h c2) rest
        | _ -> retype_error NonFunctionalConstruction

(* If ft is the type of f which itself is applied to args, *)
(* [sort_of_atomic_type] computes ft[args] which has to be a sort *)

let sort_of_atomic_type env sigma ft args =
  let rec concl_of_arity env n ar args =
    match kind_of_term (whd_all env sigma ar), args with
    | Prod (na, t, b), h::l -> concl_of_arity (push_rel (LocalDef (na, lift n h, t)) env) (n + 1) b l
    | Sort s, [] -> s
    | _ -> retype_error NotASort
  in concl_of_arity env 0 ft (Array.to_list args)

let type_of_var env id =
  try NamedDecl.get_type (lookup_named id env)
  with Not_found -> retype_error (BadVariable id)

let decomp_sort env sigma t =
  match kind_of_term (whd_all env sigma t) with
  | Sort s -> s
  | _ -> retype_error NotASort

let retype ?(polyprop=true) sigma =
  let rec type_of env cstr =
    match kind_of_term cstr with
    | Meta n ->
      (try strip_outer_cast (Evd.meta_ftype sigma n).Evd.rebus
       with Not_found -> retype_error (BadMeta n))
    | Rel n ->
	let ty = RelDecl.get_type (lookup_rel n env) in
        lift n ty
    | Var id -> type_of_var env id
    | Const cst -> rename_type_of_constant env cst
    | Evar ev -> Evd.existential_type sigma ev
    | Ind ind -> rename_type_of_inductive env ind
    | Construct cstr -> rename_type_of_constructor env cstr
    | Case (_,p,c,lf) ->
        let Inductiveops.IndType(indf,realargs) =
          let t = type_of env c in
          try Inductiveops.find_rectype env sigma t
          with Not_found ->
          try
            let t = get_type_from_constraints env sigma t in
            Inductiveops.find_rectype env sigma t
          with Not_found -> retype_error BadRecursiveType
        in
        let n = inductive_nrealdecls_env env (fst (fst (dest_ind_family indf))) in
        let t = betazetaevar_applist sigma n p realargs in
        (match kind_of_term (whd_all env sigma (type_of env t)) with
          | Prod _ -> whd_beta sigma (applist (t, [c]))
          | _ -> t)
    | Lambda (name,c1,c2) ->
          mkProd (name, c1, type_of (push_rel (LocalAssum (name,c1)) env) c2)
    | LetIn (name,b,c1,c2) ->
         subst1 b (type_of (push_rel (LocalDef (name,b,c1)) env) c2)
    | Fix ((_,i),(_,tys,_)) -> tys.(i)
    | CoFix (i,(_,tys,_)) -> tys.(i)
    | App(f,args) when is_template_polymorphic env f ->
	let t = type_of_global_reference_knowing_parameters env f args in
        strip_outer_cast (subst_type env sigma t (Array.to_list args))
    | App(f,args) ->
        strip_outer_cast
          (subst_type env sigma (type_of env f) (Array.to_list args))
    | Proj (p,c) ->
       let ty = type_of env c in
       (try
	   Inductiveops.type_of_projection_knowing_arg env sigma p c ty
	 with Invalid_argument _ -> retype_error BadRecursiveType)
    | Cast (c,_, t) -> t
    | Sort _ | Prod _ -> mkSort (sort_of env cstr)

  and sort_of env t =
    match kind_of_term t with
    | Cast (c,_, s) when isSort s -> destSort s
    | Sort s -> Sorts.super s
    | Prod (name,t,c2) ->
       let domsort = sort_of env t in
       let rangsort = sort_of (push_rel (LocalAssum (name,t)) env) c2 in
       let is_impredicative_set = is_impredicative_set env in
       Sorts.sort_of_product ~is_impredicative_set domsort rangsort
    | App(f,args) when is_template_polymorphic env f ->
      let t = type_of_global_reference_knowing_parameters env f args in
        sort_of_atomic_type env sigma t args
    | App(f,args) -> sort_of_atomic_type env sigma (type_of env f) args
    | Lambda _ | Fix _ | Construct _ -> retype_error NotAType
    | _ -> decomp_sort env sigma (type_of env t)

  and sort_family_of env t =
    match kind_of_term t with
    | Cast (c,_, s) when isSort s -> family_of_sort (destSort s)
    | Sort _ -> InType
    | Prod (name,t,c2) ->
	let s2 = sort_family_of (push_rel (LocalAssum (name,t)) env) c2 in
	if not (is_impredicative_set env) &&
	   s2 == InSet && sort_family_of env t == InType then InType else s2
    | App(f,args) when is_template_polymorphic env f ->
	let t = type_of_global_reference_knowing_parameters env f args in
        family_of_sort (sort_of_atomic_type env sigma t args)
    | App(f,args) ->
	family_of_sort (sort_of_atomic_type env sigma (type_of env f) args)
    | Lambda _ | Fix _ | Construct _ -> retype_error NotAType
    | _ -> 
      family_of_sort (decomp_sort env sigma (type_of env t))

  and type_of_global_reference_knowing_parameters env c args =
    let argtyps =
      Array.map (fun c -> lazy (nf_evar sigma (type_of env c))) args in
    match kind_of_term c with
    | Ind ind ->
      let mip = lookup_mind_specif env (fst ind) in
	(try Preinductive.type_of_inductive_knowing_parameters
	       ~polyprop env (mip,snd ind) argtyps
	 with CClosure.NotArity -> retype_error NotAnArity)
    | Const cst ->
	(try Typeops.type_of_constant_knowing_parameters_in env cst argtyps
	 with CClosure.NotArity -> retype_error NotAnArity)
    | Var id -> type_of_var env id
    | Construct cstr -> type_of_constructor env cstr
    | _ -> assert false

  in type_of, sort_of, sort_family_of,
     type_of_global_reference_knowing_parameters

let get_sort_of ?(polyprop=true) env sigma t =
  let _,f,_,_ = retype ~polyprop sigma in anomaly_on_error (f env) t
let get_sort_family_of ?(polyprop=true) env sigma c =
  let _,_,f,_ = retype ~polyprop sigma in anomaly_on_error (f env) c
let type_of_global_reference_knowing_parameters env sigma c args =
  let _,_,_,f = retype sigma in anomaly_on_error (f env c) args

let type_of_global_reference_knowing_conclusion env sigma c conclty =
  let conclty = nf_evar sigma conclty in
  match kind_of_term c with
    | Ind (ind,u) ->
        let spec = Preinductive.lookup_mind_specif env ind in
          type_of_inductive_knowing_conclusion env sigma (spec,u) conclty
    | Const cst ->
        let t = constant_type_in env cst in
        (* TODO *)
          sigma, Typeops.type_of_constant_type_knowing_parameters env t [||]
    | Var id -> sigma, type_of_var env id
    | Construct cstr -> sigma, type_of_constructor env cstr
    | _ -> assert false

(* Profiling *)
(* let get_type_of polyprop lax env sigma c = *)
(*   let f,_,_,_ = retype ~polyprop sigma in *)
(*     if lax then f env c else anomaly_on_error (f env) c  *)

(* let get_type_of_key = Profile.declare_profile "get_type_of" *)
(* let get_type_of = Profile.profile5 get_type_of_key get_type_of *)

(* let get_type_of ?(polyprop=true) ?(lax=false) env sigma c = *)
(*   get_type_of polyprop lax env sigma c *)

let get_type_of ?(polyprop=true) ?(lax=false) env sigma c =
  let f,_,_,_ = retype ~polyprop sigma in
    if lax then f env c else anomaly_on_error (f env) c

(* Makes an unsafe judgment from a constr *)
let get_judgment_of env evc c = { uj_val = c; uj_type = get_type_of env evc c }

(* Returns sorts of a context *)
let sorts_of_context env evc ctxt =
  let rec aux = function
  | [] -> env,[]
  | d :: ctxt ->
      let env,sorts = aux ctxt in
      let s = get_sort_of env evc (RelDecl.get_type d) in
      (push_rel d env,s::sorts) in
  snd (aux ctxt)

let expand_projection env sigma pr c args =
  let ty = get_type_of ~lax:true env sigma c in
  let (i,u), ind_args = 
    try Inductiveops.find_mrectype env sigma ty 
    with Not_found -> retype_error BadRecursiveType
  in
    mkApp (mkConstU (Projection.constant pr,u), 
	   Array.of_list (ind_args @ (c :: args)))
