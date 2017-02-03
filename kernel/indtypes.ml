(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

open CErrors
open Util
open Names
open Univ
open Term
open Vars
open Declarations
open Declareops
open Preinductive
open Inductive
open Environ
open CClosure
open Reduction
open Typeops
open Entries
open Pp
open Context.Rel.Declaration

(* Terminology:
paramdecls (ou paramsctxt?)
args = params + realargs (called vargs when an array, largs when a list)
params = recparams + nonrecparams
nonrecargs = nonrecparams + realargs
env_ar = initial env + declaration of inductive types
env_ar_par = env_ar + declaration of parameters
nmr = ongoing computation of recursive parameters
*)

(* Tell if indices (aka real arguments) contribute to size of inductive type *)
(* If yes, this is compatible with the univalent model *)

let indices_matter = ref false

let enforce_indices_matter () = indices_matter := true
let is_indices_matter () = !indices_matter

(* [weaker_noccur_between env n nvars t] (defined above), checks that
   no de Bruijn indices between [n] and [n+nvars] occur in [t]. If
   some such occurrences are found, then reduction is performed
   (lazily for efficiency purposes) in order to determine whether
   these occurrences are occurrences in the normal form. If the
   occurrences are eliminated a witness reduct [Some t'] of [t] is
   returned otherwise [None] is returned. *)
let weaker_noccur_between env x nvars t =
  if noccur_between x nvars t then Some t
  else
   let t' = whd_all env t in
   if noccur_between x nvars t' then Some t'
   else None

(************************************************************************)
(* Various well-formedness check for inductive declarations            *)

(* Errors related to inductive constructions *)
type inductive_error =
  | NonPos of env * constr * constr
  | NotEnoughArgs of env * constr * constr
  | NotConstructor of env * Id.t * constr * constr * int * int
  | NonPar of env * constr * int * constr * constr
  | SameNamesTypes of Id.t
  | SameNamesConstructors of Id.t
  | SameNamesOverlap of Id.t list
  | NotAnArity of env * constr
  | BadEntry
  | LargeNonPropInductiveNotInType

exception InductiveError of inductive_error

(* [check_constructors_names id s cl] checks that all the constructors names
   appearing in [l] are not present in the set [s], and returns the new set
   of names. The name [id] is the name of the current inductive type, used
   when reporting the error. *)

let check_constructors_names =
  let rec check idset = function
    | [] -> idset
    | c::cl ->
	if Id.Set.mem c idset then
	  raise (InductiveError (SameNamesConstructors c))
	else
	  check (Id.Set.add c idset) cl
  in
  check

(* [mind_check_names mie] checks the names of an inductive types declaration,
   and raises the corresponding exceptions when two types or two constructors
   have the same name. *)

let mind_check_names mie =
  let rec check indset cstset = function
    | [] -> ()
    | ind::inds ->
	let id = ind.mind_entry_typename in
	let cl = ind.mind_entry_consnames in
	if Id.Set.mem id indset then
	  raise (InductiveError (SameNamesTypes id))
	else
	  let cstset' = check_constructors_names cstset cl in
	  check (Id.Set.add id indset) cstset' inds
  in
  check Id.Set.empty Id.Set.empty mie.mind_entry_inds
(* The above verification is not necessary from the kernel point of
  vue since inductive and constructors are not referred to by their
  name, but only by the name of the inductive packet and an index. *)

(************************************************************************)
(************************************************************************)

(* Typing the arities and constructor types *)

(* An inductive definition is a "unit" if it has only one constructor
   and that all arguments expected by this constructor are
   logical, this is the case for equality, conjunction of logical properties.
   (^ enforced in infer_constructor_packet)

   Also, if the output of the unique constructor is [ind_name args],
   then the [args] must be built only from constructors and variables (both [Var] and [Rel]),
   with each variable occuring only once across the [args] as an argument to a constructor.
   (Intuition: Otherwise, multiple occurences correspond to implicit identities
    which are not trivial in strict_mode, and non-constructor non-variable
    values can hide multiple occurences, and variables applied to things are like functions.)

   TODO there may be a more relaxed/cleaner condition.
*)

type unit_conditions = AlwaysUnit | NeverUnit | CondUnit of types list

let combine_conditions c1 c2 =
  match c1, c2 with
  | AlwaysUnit, c | c, AlwaysUnit -> c
  | NeverUnit, _ | _, NeverUnit -> NeverUnit
  | CondUnit l1, CondUnit l2 -> CondUnit (List.append l1 l2)

exception NotUnitShape

(* [liftn] is the number of types being defined *)
let constructor_unit_conditions env args =
  let rec check_constr sets c =
    (* TODO strip_outer_cast, whd_all, ?? *)
    match kind_of_term c with
    | Rel n ->
       let (relset, relrepeat, varset, varrepeat) = sets in
       if Int.Set.mem n relset
       then (relset, Int.Set.add n relrepeat, varset, varrepeat)
       else (Int.Set.add n relset, relrepeat, varset, varrepeat)
    | Var x ->
       let (relset, relrepeat, varset, varrepeat) = sets in
       if Id.Set.mem x varset
       then (relset, relrepeat, varset, Id.Set.add x varrepeat)
       else (relset, relrepeat, Id.Set.add x varset, varrepeat)
    | _ ->
       let hd, args = decompose_app c in
       if isConstruct hd
       then
         List.fold_left check_constr sets args
       else raise NotUnitShape
  in
  try
    let (_, relset, _, varset) =
      List.fold_left check_constr
                     (Int.Set.empty, Int.Set.empty, Id.Set.empty, Id.Set.empty)
                     args
    in
    let tys =
      []
      |> Int.Set.fold
           (fun n tys -> (env |> lookup_rel n |> Context.Rel.Declaration.get_type |> lift n) :: tys)
           relset
      |> Id.Set.fold (fun x tys -> (named_type x env) :: tys) varset
    in
    if tys = []
    then AlwaysUnit
    else CondUnit tys
  with NotUnitShape -> NeverUnit


let infos_and_sort env t =
  let rec aux env t max =
    let t = whd_all env t in
      match kind_of_term t with
      | Prod (name,c1,c2) ->
        let varj = infer_type env c1 in
	let env1 = Environ.push_rel (LocalAssum (name,varj.utj_val)) env in
        let max = Sorts.sup max varj.utj_type in
	  aux env1 c2 max
      | _ ->
         (* We check that we can be in Prop even if max is not Prop,
            because of (template) polymorphism *)
         let _hd, args = decompose_app t in
         (* Don't check isRelN _ hd, that's in positivity. *)
         max, constructor_unit_conditions env args
    in aux env t Sorts.prop

(* Computing the levels of polymorphic inductive types

   For each inductive type of a block that is of level u_i, we have
   the constraints that u_i >= v_i where v_i is the type level of the
   types of the constructors of this inductive type. Each v_i depends
   of some of the u_i and of an extra (maybe non variable) universe,
   say w_i that summarize all the other constraints. Typically, for
   three inductive types, we could have

   u1,u2,u3,w1 <= u1
   u1       w2 <= u2
      u2,u3,w3 <= u3

   From this system of inequations, we shall deduce

   w1,w2,w3 <= u1
   w1,w2 <= u2
   w1,w2,w3 <= u3
*)

(* This (re)computes informations relevant to extraction and the sort of an
   arity or type constructor; we do not to recompute universes constraints *)

let infer_constructor_packet env_ar_par params lc =
  (* type-check the constructors *)
  let jlc = List.map (infer_type env_ar_par) lc in
  let jlc = Array.of_list jlc in
  (* generalize the constructor over the parameters *)
  let lc'' = Array.map (fun j -> it_mkProd_or_LetIn j.utj_val params) jlc in
  (* compute the max of the sorts of the products of the constructors types *)
  let levels, conds = List.split (List.map (infos_and_sort env_ar_par) lc) in
  let cond = if Array.length jlc <= 1 then AlwaysUnit else NeverUnit in
  let cond = List.fold_left combine_conditions cond conds in
  let level = List.fold_left (fun level l -> Sorts.sup level l) Sorts.prop levels in
  (lc'', level, cond)

(* If indices matter *)
let cumulate_arity_large_levels env sign =
  fst (List.fold_right
    (fun d (lev,env) ->
     match d with
     | LocalAssum (_,t) ->
	let tj = infer_type env t in
        let u = tj.utj_type in
          (Sorts.sup u lev, push_rel d env)
     | LocalDef _ ->
	lev, push_rel d env)
    sign (Sorts.prop, env))

let is_impredicative env u =
  Sorts.is_prop u || (Sorts.is_set u && is_impredicative_set env)

(* Returns the list [x_1, ..., x_n] of levels contributing to template
   polymorphism. The elements x_k is None if the k-th parameter (starting
   from the most recent and ignoring let-definitions) is not contributing 
   or is Some u_k if its level is u_k and is contributing. *)
let param_ccls paramsctxt =
  let fold (accu, acct as acc) = function
    | (LocalAssum (_, p)) ->
       let u, t =
         let c = strip_prod_assum p in
         match kind_of_term c with
         | Sort u -> Sorts.univ_level u, Sorts.trunc_level u
         | _ -> None, None
       in
       u :: accu, t :: acct
    | LocalDef _ -> acc
  in
  List.fold_left fold ([],[]) paramsctxt

let squash_from_level target =
  if Sorts.is_prop target
  then PropSquash
  else SetSquash

let squash_from_cond = function
  | AlwaysUnit -> NoSquash
  | NeverUnit -> PropSquash
  | CondUnit tys -> ConditionalSquash tys

(* Type-check an inductive definition. Does not check positivity
   conditions. *)
(* TODO check that we don't overgeneralize construcors/inductive arities with
   universes that are absent from them. Is it possible? 
*)
let typecheck_inductive env mie =
  let () = match mie.mind_entry_inds with
  | [] -> anomaly (Pp.str "empty inductive types declaration")
  | _ -> ()
  in
  (* Check unicity of names *)
  mind_check_names mie;
  (* Params are typed-checked here *)
  let env' = push_context mie.mind_entry_universes env in
  let (env_params,paramsctxt) = infer_local_decls env' mie.mind_entry_params in
  (* We first type arity of each inductive definition *)
  (* This allows building the environment of arities and to share *)
  (* the set of constraints *)
  let env_arities, rev_arity_list =
    List.fold_left
      (fun (env_ar,l) ind ->
         (* Arities (without params) are typed-checked here *)
	 let expltype = ind.mind_entry_template in
         let arity =
           let arity = infer_type env_params ind.mind_entry_arity in
           arity.utj_val
	 in
	 let (sign, deflev) = dest_arity env_params arity in
	 let inflev = 
	   (* The level of the inductive includes levels of indices if 
	      in indices_matter mode *)
	     if !indices_matter 
	     then Some (cumulate_arity_large_levels env_params sign)
	     else None
	 in
	 (* We do not need to generate the universe of full_arity; if
	    later, after the validation of the inductive definition,
	    full_arity is used as argument or subject to cast, an
	    upper universe will be generated *)
	 let full_arity = it_mkProd_or_LetIn arity paramsctxt in
	 let id = ind.mind_entry_typename in
	 let env_ar' =
           push_rel (LocalAssum (Name id, full_arity)) env_ar in
             (* (add_constraints cst2 env_ar) in *)
	   (env_ar', (id,full_arity,sign @ paramsctxt,expltype,deflev,inflev)::l))
      (env',[])
      mie.mind_entry_inds in

  let arity_list = List.rev rev_arity_list in

  (* builds the typing context "Gamma, I1:A1, ... In:An, params" *)
  let env_ar_par = push_rel_context paramsctxt env_arities in

  (* Now, we type the constructors (without params) *)
  let inds =
    List.fold_right2
      (fun ind arity_data inds ->
         let (lc',cstrs_univ,cond) =
           infer_constructor_packet env_ar_par paramsctxt ind.mind_entry_lc in
	 let consnames = ind.mind_entry_consnames in
         let ind' = (arity_data,consnames,lc',cstrs_univ,cond) in
	   ind'::inds)
      mie.mind_entry_inds
      arity_list
    ([]) in

  let inds = Array.of_list inds in

  (* Compute/check the sorts of the inductive types *)

  let inds =
    Array.map (fun ((id,full_arity,sign,expltype,def_level,inf_level),cn,lc,clev,cond)  ->
      let infu = 
	(** Inferred level, with parameters and constructors. *)
	match inf_level with
        | Some alev -> Sorts.sup clev alev
	| None -> clev
      in
      let full_polymorphic () = 
        let is_natural =
          if type_in_type env || Sorts.Graph.check_leq (universes env') infu def_level
          then if not (Sorts.is_prop def_level)
               then NoSquash
               else (* Prop has additional conditions on constructor shapes*)
                 squash_from_cond cond
          else squash_from_level def_level
        in
        let _ =
	  (** Impredicative sort, always allow *)
          if is_impredicative env def_level then ()
	  else (** Predicative case: the inferred level must be lower or equal to the
		   declared level. *)
            if is_natural <> NoSquash then
	      anomaly ~label:"check_inductive" 
		(Pp.str"Incorrect universe " ++
                   Sorts.pr def_level ++ Pp.str " declared for inductive type, inferred level is "
                 ++ Sorts.pr infu)
	in
          RegularArity (is_natural,full_arity,def_level)
      in
      let template_polymorphic () =
	let sign, s =
          try dest_arity env full_arity
          with NotArity -> raise (InductiveError (NotAnArity (env, full_arity)))
        in
        if expltype && not (Sorts.is_small s)
        then
          (* Explicitly polymorphic *)
          (* The polymorphic level is a function of the level of the *)
          (* conclusions of the parameters *)
          (* We enforce [u >= lev] in case [lev] has a strict upper *)
          (* constraints over [u] *)
          (* Template polymorphic: do not squash to Prop if constructors have bad shape *)
          let infu = match cond with
            | NeverUnit | CondUnit _ -> Sorts.sup Sorts.set infu
            | AlwaysUnit -> infu
          in
          let b = type_in_type env || Sorts.Graph.check_leq (universes env') infu s in
          if not b then
            anomaly ~label:"check_inductive"
                    (Pp.str"Incorrect universe " ++
                       Sorts.pr s ++ Pp.str " declared for inductive type, inferred level is "
                     ++ Sorts.pr clev)
          else
            TemplateArity (param_ccls paramsctxt, infu)
        else (* Not an explicit occurrence of Type *)
          full_polymorphic ()
      in
      let arity = 
	if mie.mind_entry_polymorphic then full_polymorphic ()
	else template_polymorphic ()
      in
	(id,cn,lc,(sign,arity)))
    inds
  in (env_arities, env_ar_par, paramsctxt, inds)

(************************************************************************)
(************************************************************************)
(* Positivity *)

type ill_formed_ind =
  | LocalNonPos of int
  | LocalNotEnoughArgs of int
  | LocalNotConstructor of Context.Rel.t * int
  | LocalNonPar of int * int * int

exception IllFormedInd of ill_formed_ind

(* [mind_extract_params mie] extracts the params from an inductive types
   declaration, and checks that they are all present (and all the same)
   for all the given types. *)

let mind_extract_params = decompose_prod_n_assum

let explain_ind_err id ntyp env nparamsctxt c err =
  let (lparams,c') = mind_extract_params nparamsctxt c in
  match err with
    | LocalNonPos kt ->
	raise (InductiveError (NonPos (env,c',mkRel (kt+nparamsctxt))))
    | LocalNotEnoughArgs kt ->
	raise (InductiveError
		 (NotEnoughArgs (env,c',mkRel (kt+nparamsctxt))))
    | LocalNotConstructor (paramsctxt,nargs)->
        let nparams = Context.Rel.nhyps paramsctxt in
	raise (InductiveError
		 (NotConstructor (env,id,c',mkRel (ntyp+nparamsctxt),
                                  nparams,nargs)))
    | LocalNonPar (n,i,l) ->
	raise (InductiveError
		 (NonPar (env,c',n,mkRel i,mkRel (l+nparamsctxt))))

let failwith_non_pos n ntypes c =
  for k = n to n + ntypes - 1 do
    if not (noccurn k c) then raise (IllFormedInd (LocalNonPos (k-n+1)))
  done

let failwith_non_pos_vect n ntypes v =
  Array.iter (failwith_non_pos n ntypes) v;
  anomaly ~label:"failwith_non_pos_vect" (Pp.str "some k in [n;n+ntypes-1] should occur")

let failwith_non_pos_list n ntypes l =
  List.iter (failwith_non_pos n ntypes) l;
  anomaly ~label:"failwith_non_pos_list" (Pp.str "some k in [n;n+ntypes-1] should occur")

(* Check the inductive type is called with the expected parameters *)
(* [n] is the index of the last inductive type in [env] *)
let check_correct_par (env,n,ntypes,_) paramdecls ind_index args =
  let nparams = Context.Rel.nhyps paramdecls in
  let args = Array.of_list args in
  if Array.length args < nparams then
    raise (IllFormedInd (LocalNotEnoughArgs ind_index));
  let (params,realargs) = Array.chop nparams args in
  let nparamdecls = List.length paramdecls in
  let rec check param_index paramdecl_index = function
    | [] -> ()
    | LocalDef _ :: paramdecls ->
      check param_index (paramdecl_index+1) paramdecls
    | _::paramdecls ->
        match kind_of_term (whd_all env params.(param_index)) with
	  | Rel w when Int.equal w paramdecl_index ->
            check (param_index-1) (paramdecl_index+1) paramdecls
	  | _ ->
            let paramdecl_index_in_env = paramdecl_index-n+nparamdecls+1 in
            let err =
              LocalNonPar (param_index+1, paramdecl_index_in_env, ind_index) in
            raise (IllFormedInd err)
  in check (nparams-1) (n-nparamdecls) paramdecls;
  if not (Array.for_all (noccur_between n ntypes) realargs) then
    failwith_non_pos_vect n ntypes realargs

(* Computes the maximum number of recursive parameters:
   the first parameters which are constant in recursive arguments
   [n] is the current depth, [nmr] is the maximum number of possible
   recursive parameters *)

let compute_rec_par (env,n,_,_) paramsctxt nmr largs =
if Int.equal nmr 0 then 0 else
(* start from 0, params will be in reverse order *)
  let (lpar,_) = List.chop nmr largs in
  let rec find k index =
      function
	  ([],_) -> nmr
	| (_,[]) -> assert false (* |paramsctxt|>=nmr *)
	| (lp, LocalDef _ :: paramsctxt) -> find k (index-1) (lp,paramsctxt)
	| (p::lp,_::paramsctxt) ->
       ( match kind_of_term (whd_all env p) with
	  | Rel w when Int.equal w index -> find (k+1) (index-1) (lp,paramsctxt)
          | _ -> k)
  in find 0 (n-1) (lpar,List.rev paramsctxt)

(* [env] is the typing environment
   [n] is the dB of the last inductive type
   [ntypes] is the number of inductive types in the definition
     (i.e. range of inductives is [n; n+ntypes-1])
   [lra] is the list of recursive tree of each variable
 *)
let ienv_push_var (env, n, ntypes, lra) (x,a,ra) =
  (push_rel (LocalAssum (x,a)) env, n+1, ntypes, (Norec,ra)::lra)

let ienv_push_inductive (env, n, ntypes, ra_env) ((mi,u),lrecparams) =
  let auxntyp = 1 in
  let specif = (lookup_mind_specif env mi, u) in
  let ty = type_of_inductive env specif in
  let env' =
    let decl = LocalAssum (Anonymous, hnf_prod_applist env ty lrecparams) in
    push_rel decl env in
  let ra_env' =
    (Imbr mi,(Rtree.mk_rec_calls 1).(0)) ::
    List.map (fun (r,t) -> (r,Rtree.lift 1 t)) ra_env in
  (* New index of the inductive types *)
  let newidx = n + auxntyp in
  (env', newidx, ntypes, ra_env')

let rec ienv_decompose_prod (env,_,_,_ as ienv) n c =
  if Int.equal n 0 then (ienv,c) else
    let c' = whd_all env c in
    match kind_of_term c' with
	Prod(na,a,b) ->
	  let ienv' = ienv_push_var ienv (na,a,mk_norec) in
	  ienv_decompose_prod ienv' (n-1) b
      | _ -> assert false

let array_min nmr a = if Int.equal nmr 0 then 0 else
  Array.fold_left (fun k (nmri,_) -> min k nmri) nmr a

(** [check_positivity_one ienv paramsctxt (mind,i) nnonrecargs lcnames indlc]
    checks the positivity of the [i]-th member of the mutually
    inductive definition [mind]. It returns an [Rtree.t] which
    represents the position of the recursive calls of inductive in [i]
    for use by the guard condition (terms at these positions are
    considered sub-terms) as well as the number of of non-uniform
    arguments (used to generate induction schemes, so a priori less
    relevant to the kernel).

    If [chkpos] is [false] then positivity is assumed, and
    [check_positivity_one] computes the subterms occurrences in a
    best-effort fashion. *)
let check_positivity_one ~chkpos recursive (env,_,ntypes,_ as ienv) paramsctxt (_,i as ind) nnonrecargs lcnames indlc =
  let nparamsctxt = Context.Rel.length paramsctxt in
  let nmr = Context.Rel.nhyps paramsctxt in
  (** Positivity of one argument [c] of a constructor (i.e. the
      constructor [cn] has a type of the shape [… -> c … -> P], where,
      more generally, the arrows may be dependent). *)
  let rec check_pos (env, n, ntypes, ra_env as ienv) nmr c =
    let x,largs = decompose_app (whd_all env c) in
      match kind_of_term x with
	| Prod (na,b,d) ->
	    let () = assert (List.is_empty largs) in
            (** If one of the inductives of the mutually inductive
                block occurs in the left-hand side of a product, then
                such an occurrence is a non-strictly-positive
                recursive call. Occurrences in the right-hand side of
                the product must be strictly positive.*)
            (match weaker_noccur_between env n ntypes b with
	      | None when chkpos ->
                  failwith_non_pos_list n ntypes [b]
              | None ->
                  check_pos (ienv_push_var ienv (na, b, mk_norec)) nmr d
              | Some b ->
                  check_pos (ienv_push_var ienv (na, b, mk_norec)) nmr d)
	| Rel k ->
            (try let (ra,rarg) = List.nth ra_env (k-1) in
            let largs = List.map (whd_all env) largs in
	    let nmr1 =
	      (match ra with
                  Mrec _ -> compute_rec_par ienv paramsctxt nmr largs
		|  _ -> nmr)
	    in
              (** The case where one of the inductives of the mutually
                  inductive block occurs as an argument of another is not
                  known to be safe. So Coq rejects it. *)
	      if chkpos &&
                 not (List.for_all (noccur_between n ntypes) largs)
	      then failwith_non_pos_list n ntypes largs
	      else (nmr1,rarg)
              with Failure _ | Invalid_argument _ -> (nmr,mk_norec))
	| Ind ind_kn ->
            (** If one of the inductives of the mutually inductive
                block being defined appears in a parameter, then we
                have a nested inductive type. The positivity is then
                discharged to the [check_positive_nested] function. *)
            if List.for_all (noccur_between n ntypes) largs then (nmr,mk_norec)
            else check_positive_nested ienv nmr (ind_kn, largs)
	| err ->
            (** If an inductive of the mutually inductive block
                appears in any other way, then the positivy check gives
                up. *)
	    if not chkpos ||
              (noccur_between n ntypes x &&
               List.for_all (noccur_between n ntypes) largs)
	    then (nmr,mk_norec)
	    else failwith_non_pos_list n ntypes (x::largs)

  (** [check_positive_nested] handles the case of nested inductive
      calls, that is, when an inductive types from the mutually
      inductive block is called as an argument of an inductive types
      (for the moment, this inductive type must be a previously
      defined types, not one of the types of the mutually inductive
      block being defined). *)
  (* accesses to the environment are not factorised, but is it worth? *)
  and check_positive_nested (env,n,ntypes,ra_env as ienv) nmr ((mi,u), largs) =
    let (mib,mip) = lookup_mind_specif env mi in
    let auxnrecpar = mib.mind_nparams_rec in
    let auxnnonrecpar = mib.mind_nparams - auxnrecpar in
    let (auxrecparams,auxnonrecargs) =
      try List.chop auxnrecpar largs
      with Failure _ -> raise (IllFormedInd (LocalNonPos n)) in

      (** Inductives of the inductive block being defined are only
          allowed to appear nested in the parameters of another inductive
          type. Not in the proper indices. *)
      if chkpos && not (List.for_all (noccur_between n ntypes) auxnonrecargs) then
	failwith_non_pos_list n ntypes auxnonrecargs;
      (* Nested mutual inductive types are not supported *)
      let auxntyp = mib.mind_ntypes in
	if not (Int.equal auxntyp 1) then raise (IllFormedInd (LocalNonPos n));
	(* The nested inductive type with parameters removed *)
	let auxlcvect = abstract_mind_lc auxntyp auxnrecpar mip.mind_nf_lc in
	  (* Extends the environment with a variable corresponding to
	     the inductive def *)
	let (env',_,_,_ as ienv') = ienv_push_inductive ienv ((mi,u),auxrecparams) in
	  (* Parameters expressed in env' *)
	let auxrecparams' = List.map (lift auxntyp) auxrecparams in
	let irecargs_nmr =
	  (** Checks that the "nesting" inductive type is covariant in
	      the relevant parameters. In other words, that the
	      (nested) parameters which are instantiated with
	      inductives of the mutually inductive block occur
	      positively in the types of the nested constructors. *)
	  Array.map
	    (function c ->
	      let c' = hnf_prod_applist env' c auxrecparams' in
	      (* skip non-recursive parameters *)
	      let (ienv',c') = ienv_decompose_prod ienv' auxnnonrecpar c' in
		check_constructors ienv' false nmr c')
	    auxlcvect
	in
	let irecargs = Array.map snd irecargs_nmr
	and nmr' = array_min nmr irecargs_nmr
	in
	  (nmr',(Rtree.mk_rec [|mk_paths (Imbr mi) irecargs|]).(0))

  (** [check_constructors ienv check_head nmr c] checks the positivity
      condition in the type [c] of a constructor (i.e. that recursive
      calls to the inductives of the mutually inductive definition
      appear strictly positively in each of the arguments of the
      constructor, see also [check_pos]). If [check_head] is [true],
      then the type of the fully applied constructor (the "head" of
      the type [c]) is checked to be the right (properly applied)
      inductive type. *)
  and check_constructors ienv check_head nmr c =
    let rec check_constr_rec (env,n,ntypes,ra_env as ienv) nmr lrec c =
      let x,largs = decompose_app (whd_all env c) in
	match kind_of_term x with

          | Prod (na,b,d) ->
	      let () = assert (List.is_empty largs) in
	      if not recursive && not (noccur_between n ntypes b) then
	        raise (InductiveError BadEntry);
              let nmr',recarg = check_pos ienv nmr b in
              let ienv' = ienv_push_var ienv (na,b,mk_norec) in
                check_constr_rec ienv' nmr' (recarg::lrec) d
          | hd ->
            let () =
              if check_head then
                begin match hd with
                | Rel j when Int.equal j (n + ntypes - i - 1) ->
                  check_correct_par ienv paramsctxt (ntypes - i) largs
                | _ -> raise (IllFormedInd (LocalNotConstructor(paramsctxt,nnonrecargs)))
                end
              else
                if chkpos &&
                   not (List.for_all (noccur_between n ntypes) largs)
                then failwith_non_pos_list n ntypes largs
            in
            (nmr, List.rev lrec)
    in check_constr_rec ienv nmr [] c
  in
  let irecargs_nmr =
    Array.map2
      (fun id c ->
        let _,rawc = mind_extract_params nparamsctxt c in
          try
	    check_constructors ienv true nmr rawc
          with IllFormedInd err ->
	    explain_ind_err id (ntypes-i) env nparamsctxt c err)
      (Array.of_list lcnames) indlc
  in
  let irecargs = Array.map snd irecargs_nmr
  and nmr' = array_min nmr irecargs_nmr
  in (nmr', mk_paths (Mrec ind) irecargs)

(** [check_positivity ~chkpos kn env_ar paramsctxt inds] checks that the mutually
    inductive block [inds] is strictly positive.

    If [chkpos] is [false] then positivity is assumed, and
    [check_positivity_one] computes the subterms occurrences in a
    best-effort fashion. *)
let check_positivity ~chkpos kn env_ar_par paramsctxt finite inds =
  let ntypes = Array.length inds in
  let recursive = finite != Decl_kinds.BiFinite in
  let rc = Array.mapi (fun j t -> (Mrec (kn,j),t)) (Rtree.mk_rec_calls ntypes) in
  let ra_env_ar = Array.rev_to_list rc in
  let nparamsctxt = Context.Rel.length paramsctxt in
  let nmr = Context.Rel.nhyps paramsctxt in
  let check_one i (_,lcnames,lc,(sign,_)) =
    let ra_env_ar_par =
      List.init nparamsctxt (fun _ -> (Norec,mk_norec)) @ ra_env_ar in
    let ienv = (env_ar_par, 1+nparamsctxt, ntypes, ra_env_ar_par) in
    let nnonrecargs = Context.Rel.nhyps sign - nmr in
    check_positivity_one ~chkpos recursive ienv paramsctxt (kn,i) nnonrecargs lcnames lc
  in
  let irecargs_nmr = Array.mapi check_one inds in
  let irecargs = Array.map snd irecargs_nmr
  and nmr' = array_min nmr irecargs_nmr
  in (nmr',Rtree.mk_rec irecargs)


(************************************************************************)
(************************************************************************)
(* Build the inductive packet *)

(* Allowed eliminations *)

let allowed_sorts is_smashed s =
  if not is_smashed 
  then (** Naturally in the defined sort.
	   If [s] is Prop, it must be small and unitary.
	   Unsmashed, predicative Type and Set: all elimination allowed
	   as well. *)
      InType
  else family_of_sort s
    
(* Previous comment: *)
(* Unitary/empty Prop: elimination to all sorts are realizable *)
(* unless the type is large. If it is large, forbids large elimination *)
(* which otherwise allows simulating the inconsistent system Type:Type. *)
(* -> this is now handled by is_smashed: *)
(* - all_sorts in case of small, unitary Prop (not smashed) *)
(* - logical_sorts in case of large, unitary Prop (smashed) *)

let arity_conclusion = function
  | RegularArity (_, c, _) -> c
  | TemplateArity (_, s) -> mkSort s

let fold_inductive_blocks f =
  Array.fold_left (fun acc (_,_,lc,(arsign,ar)) ->
    f (Array.fold_left f acc lc) (it_mkProd_or_LetIn (arity_conclusion ar) arsign))

let used_section_variables env inds =
  let ids = fold_inductive_blocks
    (fun l c -> Id.Set.union (Environ.global_vars_set env c) l)
      Id.Set.empty inds in
  keep_hyps env ids

let rel_vect n m = Array.init m (fun i -> mkRel(n+m-i))
let rel_list n m = Array.to_list (rel_vect n m)

exception UndefinableExpansion

(** From a rel context describing the constructor arguments,
    build an expansion function.
    The term built is expecting to be substituted first by 
    a substitution of the form [params, x : ind params] *)
let compute_projections ((kn, _ as ind), u as indu) n x nparamargs params
    mind_consnrealdecls mind_consnrealargs paramslet ctx =
  let mp, dp, l = repr_mind kn in
  (** We build a substitution smashing the lets in the record parameters so
      that typechecking projections requires just a substitution and not
      matching with a parameter context. *)
  let indty, paramsletsubst =
    (* [ty] = [Ind inst] is typed in context [params] *)
    let inst = Context.Rel.to_extended_vect 0 paramslet in
    let ty = mkApp (mkIndU indu, inst) in
    (* [Ind inst] is typed in context [params-wo-let] *)
    let inst' = rel_list 0 nparamargs in
    (* {params-wo-let |- subst:params] *)
    let subst = subst_of_rel_context_instance paramslet inst' in
    (* {params-wo-let, x:Ind inst' |- subst':(params,x:Ind inst)] *)
    let subst = (* For the record parameter: *)
      mkRel 1 :: List.map (lift 1) subst in
      ty, subst
  in
  let ci = 
    let print_info =
      { ind_tags = []; cstr_tags = [|Context.Rel.to_tags ctx|]; style = LetStyle } in
      { ci_ind     = ind;
	ci_npar    = nparamargs;
	ci_cstr_ndecls = mind_consnrealdecls;
	ci_cstr_nargs = mind_consnrealargs;
	ci_pp_info = print_info }
  in
  let len = List.length ctx in
  let x = Name x in
  let compat_body ccl i = 
    (* [ccl] is defined in context [params;x:indty] *)
    (* [ccl'] is defined in context [params;x:indty;x:indty] *)
    let ccl' = liftn 1 2 ccl in
    let p = mkLambda (x, lift 1 indty, ccl') in
    let branch = it_mkLambda_or_LetIn (mkRel (len - i)) ctx in
    let body = mkCase (ci, p, mkRel 1, [|lift 1 branch|]) in
      it_mkLambda_or_LetIn (mkLambda (x,indty,body)) params
  in
  let projections decl (i, j, kns, pbs, subst, letsubst) =
    match decl with
    | LocalDef (na,c,t) ->
        (* From [params, field1,..,fieldj |- c(params,field1,..,fieldj)]
           to [params, x:I, field1,..,fieldj |- c(params,field1,..,fieldj)] *)
        let c = liftn 1 j c in
        (* From [params, x:I, field1,..,fieldj |- c(params,field1,..,fieldj)]
           to [params, x:I |- c(params,proj1 x,..,projj x)] *)
        let c1 = substl subst c in
        (* From [params, x:I |- subst:field1,..,fieldj]
           to [params, x:I |- subst:field1,..,fieldj+1] where [subst]
           is represented with instance of field1 last *)
        let subst = c1 :: subst in
        (* From [params, x:I, field1,..,fieldj |- c(params,field1,..,fieldj)]
           to [params-wo-let, x:I |- c(params,proj1 x,..,projj x)] *)
        let c2 = substl letsubst c in
        (* From [params-wo-let, x:I |- subst:(params, x:I, field1,..,fieldj)]
           to [params-wo-let, x:I |- subst:(params, x:I, field1,..,fieldj+1)] *)
        let letsubst = c2 :: letsubst in
        (i, j+1, kns, pbs, subst, letsubst)
    | LocalAssum (na,t) ->
      match na with
      | Name id ->
	let kn = Constant.make1 (KerName.make mp dp (Label.of_id id)) in
        (* from [params, field1,..,fieldj |- t(params,field1,..,fieldj)]
           to [params, x:I, field1,..,fieldj |- t(params,field1,..,fieldj] *)
        let t = liftn 1 j t in
        (* from [params, x:I, field1,..,fieldj |- t(params,field1,..,fieldj)]
           to [params-wo-let, x:I |- t(params,proj1 x,..,projj x)] *)
	let projty = substl letsubst t in
        (* from [params, x:I, field1,..,fieldj |- t(field1,..,fieldj)]
           to [params, x:I |- t(proj1 x,..,projj x)] *)
	let ty = substl subst t in
	let term = mkProj (Projection.make kn true, mkRel 1) in
	let fterm = mkProj (Projection.make kn false, mkRel 1) in
	let compat = compat_body ty (j - 1) in
	let etab = it_mkLambda_or_LetIn (mkLambda (x, indty, term)) params in
	let etat = it_mkProd_or_LetIn (mkProd (x, indty, ty)) params in
	let body = { proj_ind = fst ind; proj_npars = nparamargs;
		     proj_arg = i; proj_type = projty; proj_eta = etab, etat; 
		     proj_body = compat } in
	  (i + 1, j + 1, kn :: kns, body :: pbs,
	   fterm :: subst, fterm :: letsubst)
      | Anonymous -> raise UndefinableExpansion
  in
  let (_, _, kns, pbs, subst, letsubst) =
    List.fold_right projections ctx (0, 1, [], [], [], paramsletsubst)
  in
    Array.of_list (List.rev kns),
    Array.of_list (List.rev pbs)

let build_inductive env p prv ctx env_ar paramsctxt kn isrecord isfinite inds nmr recargs =
  let ntypes = Array.length inds in
  (* Compute the set of used section variables *)
  let hyps = used_section_variables env inds in
  let nparamargs = Context.Rel.nhyps paramsctxt in
  let nparamsctxt = Context.Rel.length paramsctxt in
  let subst, ctx = Sorts.abstract_sorts p ctx in
  let paramsctxt = Vars.subst_univs_level_context subst paramsctxt in
  let env_ar = 
    let ctx = Environ.rel_context env_ar in 
    let ctx' = Vars.subst_univs_level_context subst ctx in
      Environ.push_rel_context ctx' env
  in
  (* Check one inductive *)
  let build_one_packet (id,cnames,lc,(ar_sign,ar_kind)) recarg =
    (* Type of constructors in normal form *)
    let lc = Array.map (Vars.subst_univs_level_constr subst) lc in
    let splayed_lc = Array.map (dest_prod_assum env_ar) lc in
    let nf_lc = Array.map (fun (d,b) -> it_mkProd_or_LetIn b d) splayed_lc in
    let consnrealdecls =
      Array.map (fun (d,_) -> Context.Rel.length d - nparamsctxt)
	splayed_lc in
    let consnrealargs =
      Array.map (fun (d,_) -> Context.Rel.nhyps d - nparamargs)
	splayed_lc in
    (* Elimination sorts *)
    let arkind,kelim = 
      match ar_kind with
      | TemplateArity ((ulvls, tlvls), lev) ->
	 let ar = {template_param_univ_levels = ulvls;
                   template_param_trunc_levels = tlvls;
                   template_level = lev} in
          TemplateArity ar, NoSquash
      | RegularArity (squash,ar,defs) ->
        let ar = RegularArity
	  { mind_user_arity = Vars.subst_univs_level_constr subst ar; 
            mind_sort = Sorts.level_subst_sorts subst defs; } in
          ar, squash in
    (* Assigning VM tags to constructors *)
    let nconst, nblock = ref 0, ref 0 in
    let transf num =
      let arity = List.length (dest_subterms recarg).(num) in
	if Int.equal arity 0 then
	  let p  = (!nconst, 0) in
	    incr nconst; p
	else
	  let p = (!nblock + 1, arity) in
	    incr nblock; p
	      (* les tag des constructeur constant commence a 0,
		 les tag des constructeur non constant a 1 (0 => accumulator) *)
    in
    let rtbl = Array.init (List.length cnames) transf in
      (* Build the inductive packet *)
      { mind_typename = id;
	mind_arity = arkind;
	mind_arity_ctxt = Vars.subst_univs_level_context subst ar_sign;
	mind_nrealargs = Context.Rel.nhyps ar_sign - nparamargs;
	mind_nrealdecls = Context.Rel.length ar_sign - nparamsctxt;
	mind_kelim = kelim;
	mind_consnames = Array.of_list cnames;
	mind_consnrealdecls = consnrealdecls;
	mind_consnrealargs = consnrealargs;
	mind_user_lc = lc;
	mind_nf_lc = nf_lc;
	mind_recargs = recarg;
	mind_nb_constant = !nconst;
	mind_nb_args = !nblock;
	mind_reloc_tbl = rtbl;
      } in
  let packets = Array.map2 build_one_packet inds recargs in
  let pkt = packets.(0) in
  let isrecord = 
    match isrecord with
    | Some (Some rid) when pkt.mind_kelim == NoSquash
			   && Array.length pkt.mind_consnames == 1
			   && pkt.mind_consnrealargs.(0) > 0 ->
      (** The elimination criterion ensures that all projections can be defined. *)
      let u = 
	if p then 
	  Sorts.Instance.apply_subst (Sorts.level_subst_fn subst) (Sorts.UContext.instance ctx)
	else Sorts.Instance.empty
      in
      let indsp = ((kn, 0), u) in
      let rctx, indty = decompose_prod_assum (subst1 (mkIndU indsp) pkt.mind_nf_lc.(0)) in
	(try 
	   let fields, paramslet = List.chop pkt.mind_consnrealdecls.(0) rctx in
	   let kns, projs = 
	     compute_projections indsp pkt.mind_typename rid nparamargs paramsctxt
	       pkt.mind_consnrealdecls pkt.mind_consnrealargs paramslet fields
	   in Some (Some (rid, kns, projs))
	 with UndefinableExpansion -> Some None)
    | Some _ -> Some None
    | None -> None
  in
    (* Build the mutual inductive *)
    { mind_record = isrecord;
      mind_ntypes = ntypes;
      mind_finite = isfinite;
      mind_hyps = hyps;
      mind_nparams = nparamargs;
      mind_nparams_rec = nmr;
      mind_params_ctxt = paramsctxt;
      mind_packets = packets;
      mind_polymorphic = p;
      mind_universes = ctx;
      mind_private = prv;
      mind_typing_flags = Environ.typing_flags env;
    }

(************************************************************************)
(************************************************************************)

let check_inductive env kn mie =
  (* First type-check the inductive definition *)
  let (env_ar, env_ar_par, paramsctxt, inds) = typecheck_inductive env mie in
  (* Then check positivity conditions *)
  let chkpos = (Environ.typing_flags env).check_guarded in
  let (nmr,recargs) = check_positivity ~chkpos kn env_ar_par paramsctxt mie.mind_entry_finite inds in
  (* Build the inductive packets *)
    build_inductive env mie.mind_entry_polymorphic mie.mind_entry_private
      mie.mind_entry_universes
      env_ar paramsctxt kn mie.mind_entry_record mie.mind_entry_finite
      inds nmr recargs
