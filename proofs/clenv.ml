(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

module CVars = Vars

open CErrors
open Util
open Names
open Nameops
open Termops
open Constr
open Context
open Environ
open Evd
open EConstr
open Vars
open Reduction
open Reductionops
open Tacred
open Pretype_errors
open Evarutil
open Unification
open Tactypes
open Logic

(******************************************************************)
(* Clausal environments *)

type clausenv = {
  env      : env;
  evd      : evar_map;
  metas    : metavariable list;
  templval : constr freelisted;
  templtyp : constr freelisted;
  cache : Reductionops.meta_instance_subst;
}

let mk_clausenv env evd metas templval templtyp = {
  env; evd; metas; templval; templtyp; cache = create_meta_instance_subst evd;
}

let update_clenv_evd clenv evd =
  mk_clausenv clenv.env evd clenv.metas clenv.templval clenv.templtyp

let clenv_convert_val f clenv =
  let templval = Evd.map_fl (fun c -> f clenv.env clenv.evd c) clenv.templval in
  mk_clausenv clenv.env clenv.evd clenv.metas templval clenv.templtyp

let clenv_refresh env sigma ctx clenv =
  let evd = Evd.meta_merge (Evd.meta_list clenv.evd) (Evd.clear_metas sigma) in
  match ctx with
  | Some ctx ->
    let (subst, ctx) = UnivGen.fresh_universe_context_set_instance ctx in
    let emap c = Vars.subst_univs_level_constr subst c in
    let evd = Evd.merge_context_set Evd.univ_flexible evd ctx in
    (* Only metas are mentioning the old universes. *)
    mk_clausenv env (Evd.map_metas emap evd) clenv.metas
      (Evd.map_fl emap clenv.templval)
      (Evd.map_fl emap clenv.templtyp)
  | None ->
    mk_clausenv env evd clenv.metas clenv.templval clenv.templtyp

let cl_env ce = ce.env
let clenv_evd ce =  ce.evd
let clenv_templtyp c = c.templtyp
let clenv_arguments c = c.metas

let clenv_meta_type clenv mv =
  let ty =
    try Evd.meta_ftype clenv.evd mv
    with Not_found -> anomaly Pp.(str "unknown meta ?" ++ str (Nameops.string_of_meta mv) ++ str ".") in
  meta_instance clenv.env clenv.cache ty
let clenv_value clenv = meta_instance clenv.env clenv.cache clenv.templval
let clenv_type clenv = meta_instance clenv.env clenv.cache clenv.templtyp

let clenv_hnf_constr ce t = hnf_constr (cl_env ce) (clenv_evd ce) t

let clenv_get_type_of ce c = Retyping.get_type_of (cl_env ce) (clenv_evd ce) c

let clenv_push_prod cl =
  let typ = whd_all (cl_env cl) (clenv_evd cl) (clenv_type cl) in
  let rec clrec typ = match EConstr.kind cl.evd typ with
    | Cast (t,_,_) -> clrec t
    | Prod (na,t,u) ->
        let mv = new_meta () in
        let dep = not (noccurn (clenv_evd cl) 1 u) in
        let na' = if dep then na.binder_name else Anonymous in
        let e' = meta_declare mv t ~name:na' cl.evd in
        let concl = if dep then subst1 (mkMeta mv) u else u in
        let def = applist (cl.templval.rebus,[mkMeta mv]) in
        Some (mv, dep, { templval = mk_freelisted def;
          templtyp = mk_freelisted concl;
          evd = e';
          env = cl.env;
          metas = cl.metas @ [mv];
          cache = create_meta_instance_subst e' })
    | _ -> None
  in clrec typ

(* Instantiate the first [bound] products of [t] with metas (all products if
   [bound] is [None]; unfold local defs *)

(** [clenv_environments sigma n t] returns [sigma',lmeta,ccl] where
   [lmetas] is a list of metas to be applied to a proof of [t] so that
   it produces the unification pattern [ccl]; [sigma'] is [sigma]
   extended with [lmetas]; if [n] is defined, it limits the size of
   the list even if [ccl] is still a product; otherwise, it stops when
   [ccl] is not a product; example: if [t] is [forall x y, x=y -> y=x]
   and [n] is [None], then [lmetas] is [Meta n1;Meta n2;Meta n3] and
   [ccl] is [Meta n1=Meta n2]; if [n] is [Some 1], [lmetas] is [Meta n1]
   and [ccl] is [forall y, Meta n1=y -> y=Meta n1] *)

let clenv_environments evd bound t =
  let open EConstr in
  let open Vars in
  let rec clrec (e,metas) n t =
    match n, EConstr.kind evd t with
      | (Some 0, _) -> (e, List.rev metas, t)
      | (n, Cast (t,_,_)) -> clrec (e,metas) n t
      | (n, Prod (na,t1,t2)) ->
          let mv = new_meta () in
          let dep = not (noccurn evd 1 t2) in
          let na' = if dep then na.binder_name else Anonymous in
          let e' = meta_declare mv t1 ~name:na' e in
          clrec (e', (mv)::metas) (Option.map ((+) (-1)) n)
            (if dep then (subst1 (mkMeta mv) t2) else t2)
      | (n, LetIn (na,b,_,t)) -> clrec (e,metas) n (subst1 b t)
      | (n, _) -> (e, List.rev metas, t)
  in
  clrec (evd,[]) bound t

let mk_clenv_from_env env sigma n (c,cty) =
  let evd = clear_metas sigma in
  let (evd,args,concl) = clenv_environments evd n cty in
  { templval = mk_freelisted (mkApp (c,Array.map_of_list mkMeta args));
    templtyp = mk_freelisted concl;
    evd = evd;
    env = env;
    metas = args;
    cache = create_meta_instance_subst evd }

let mk_clenv_from env sigma c = mk_clenv_from_env env sigma None c
let mk_clenv_from_n env sigma n c = mk_clenv_from_env env sigma (Some n) c

(******************************************************************)

(* [mentions clenv mv0 mv1] is true if mv1 is defined and mentions
 * mv0, or if one of the free vars on mv1's freelist mentions
 * mv0 *)

let mentions clenv mv0 =
  let rec menrec mv1 =
    Int.equal mv0 mv1 ||
    let mlist =
      try match meta_opt_fvalue clenv.evd mv1 with
      | Some (b,_) -> b.freemetas
      | None -> Metaset.empty
      with Not_found -> Metaset.empty in
    Metaset.exists menrec mlist
  in menrec

let error_incompatible_inst clenv mv  =
  let na = meta_name clenv.evd mv in
  match na with
  | Name id ->
    user_err
      Pp.(str "An incompatible instantiation has already been found for " ++
          Id.print id)
  | _ ->
    anomaly ~label:"clenv_assign" (Pp.str "non dependent metavar already assigned.")

(* TODO: replace by clenv_unify (mkMeta mv) rhs ? *)
let clenv_assign mv rhs clenv =
  let rhs_fls = mk_freelisted rhs in
  if Metaset.exists (mentions clenv mv) rhs_fls.freemetas then
    user_err Pp.(str "clenv_assign: circularity in unification");
  try
    begin match meta_opt_fvalue clenv.evd mv with
    | Some (body, _) ->
      if not (EConstr.eq_constr clenv.evd body.rebus rhs) then
        error_incompatible_inst clenv mv
      else
        clenv
    | None ->
      let st = (Conv,TypeNotProcessed) in
      update_clenv_evd clenv (meta_assign mv (rhs_fls.rebus,st) clenv.evd)
    end
  with Not_found ->
    user_err Pp.(str "clenv_assign: undefined meta")



(* [clenv_dependent hyps_only clenv]
 * returns a list of the metavars which appear in the template of clenv,
 * and which are dependent, This is computed by taking the metavars of the
 * template in right-to-left order, and collecting the metavars which appear
 * in their types, and adding in all the metavars appearing in the
 * type of clenv.
 * If [hyps_only] then metavariables occurring in the concl are _excluded_
 * If [iter] is also set then all metavariables *recursively* occurring
 * in the concl are _excluded_

   Details of the strategies used for computing the set of unresolved
   dependent metavariables

   We typically have a clause of the form

   lem(?T:Type,?T,?U:Type,?V:Type,?x:?T,?y:?U,?z:?V,?H:hyp(?x,?z)) :concl(?y,?z)

   Then, we compute:
   A  = the set of all unresolved metas
   C  = the set of metas occurring in concl (here ?y, ?z)
   C* = the recursive closure of C wrt types (here ?y, ?z, ?U, ?V)
   D  = the set of metas occurring in a type of meta (here ?x, ?T, ?z, ?U, ?V)
   NL = the set of duplicated metas even if non dependent (here ?T)
       (we make the assumption that duplicated metas have internal dependencies)

   Then, for the "apply"-style tactic (hyps_only), missing metas are
     A inter ((D minus C) union NL)

   for the optimized "apply"-style tactic (taking in care, f_equal style
   lemma, from 2/8/10, Coq > 8.3), missing metas are
     A inter (( D minus C* ) union NL)

   for the "elim"-style tactic, missing metas are
     A inter (D union C union NL)

   In any case, we respect the order given in A.
*)

let clenv_metas_in_type_of_meta clenv mv =
  (mk_freelisted (meta_instance clenv.env clenv.cache (meta_ftype clenv.evd mv))).freemetas

let dependent_in_type_of_metas clenv mvs =
  List.fold_right
    (fun mv -> Metaset.union (clenv_metas_in_type_of_meta clenv mv))
    mvs Metaset.empty

let dependent_closure clenv mvs =
  let rec aux mvs acc =
    Metaset.fold
      (fun mv deps ->
        let metas_of_meta_type = clenv_metas_in_type_of_meta clenv mv in
        aux metas_of_meta_type (Metaset.union deps metas_of_meta_type))
      mvs acc in
  aux mvs mvs

let undefined_metas evd =
  let fold n b accu = match b with
  | Clval(_,_,typ) -> accu
  | Cltyp (_,typ)  -> n :: accu
  in
  let m = Metamap.fold fold (Evd.meta_list evd) [] in
  List.sort Int.compare m

let clenv_dependent_gen hyps_only ?(iter=true) clenv =
  let all_undefined = undefined_metas clenv.evd in
  let deps_in_concl = (mk_freelisted (clenv_type clenv)).freemetas in
  let deps_in_hyps = dependent_in_type_of_metas clenv all_undefined in
  let deps_in_concl =
    if hyps_only && iter then dependent_closure clenv deps_in_concl
    else deps_in_concl in
  List.filter
    (fun mv ->
      if hyps_only then
        Metaset.mem mv deps_in_hyps && not (Metaset.mem mv deps_in_concl)
      else
        Metaset.mem mv deps_in_hyps || Metaset.mem mv deps_in_concl)
    all_undefined

let clenv_missing ce = clenv_dependent_gen true ce
let clenv_dependent ce = clenv_dependent_gen false ce

(******************************************************************)

(* Instantiate metas that create beta/iota redexes *)

let meta_reducible_instance env evd b =
  let fm = b.freemetas in
  let fold mv accu =
    let fvalue = try meta_opt_fvalue evd mv with Not_found -> None in
    match fvalue with
    | None -> accu
    | Some (g, (_, s)) -> Metamap.add mv (g.rebus, s) accu
  in
  let metas = Metaset.fold fold fm Metamap.empty in
  let rec irec u =
    let u = whd_betaiota env Evd.empty u (* FIXME *) in
    match EConstr.kind evd u with
    | Case (ci,u,pms,p,iv,c,bl) when EConstr.isMeta evd (strip_outer_cast evd c) ->
        let m = destMeta evd (strip_outer_cast evd c) in
        (match
          try
            let g, s = Metamap.find m metas in
            let is_coerce = match s with CoerceToType -> true | _ -> false in
            if isConstruct evd g || not is_coerce then Some g else None
          with Not_found -> None
          with
            | Some g -> irec (mkCase (ci,u,pms,p,iv,g,bl))
            | None ->
              let on_ctx (na, c) = (na, irec c) in
              mkCase (ci,u,Array.map irec pms,on_ctx p,iv,c,Array.map on_ctx bl))
    | App (f,l) when EConstr.isMeta evd (strip_outer_cast evd f) ->
        let m = destMeta evd (strip_outer_cast evd f) in
        (match
          try
            let g, s = Metamap.find m metas in
            let is_coerce = match s with CoerceToType -> true | _ -> false in
            if isLambda evd g || not is_coerce then Some g else None
          with Not_found -> None
         with
           | Some g -> irec (mkApp (g,l))
           | None -> mkApp (f,Array.map irec l))
    | Meta m ->
        (try let g, s = Metamap.find m metas in
          let is_coerce = match s with CoerceToType -> true | _ -> false in
          if not is_coerce then irec g else u
         with Not_found -> u)
    | Proj (p,c) when isMeta evd c || isCast evd c && isMeta evd (pi1 (destCast evd c)) (* What if two nested casts? *) ->
      let m = try destMeta evd c with DestKO -> destMeta evd (pi1 (destCast evd c)) (* idem *) in
          (match
          try
            let g, s = Metamap.find m metas in
            let is_coerce = match s with CoerceToType -> true | _ -> false in
            if isConstruct evd g || not is_coerce then Some g else None
          with Not_found -> None
          with
            | Some g -> irec (mkProj (p,g))
            | None -> mkProj (p,c))
    | _ -> EConstr.map evd irec u
  in
  if Metaset.is_empty fm then (* nf_betaiota? *) b.rebus
  else irec b.rebus

let clenv_unify ?(flags=default_unify_flags ()) cv_pb t1 t2 clenv =
  update_clenv_evd clenv (w_unify ~flags clenv.env clenv.evd cv_pb t1 t2)

let clenv_unify_meta_types ?(flags=default_unify_flags ()) clenv =
  update_clenv_evd clenv (w_unify_meta_types ~flags:flags clenv.env clenv.evd)

let clenv_unique_resolver ?(flags=default_unify_flags ()) clenv concl =
  if isMeta clenv.evd (fst (decompose_app_vect clenv.evd (whd_nored clenv.env clenv.evd clenv.templtyp.rebus))) then
    clenv_unify CUMUL ~flags (clenv_type clenv) concl
      (clenv_unify_meta_types ~flags clenv)
  else
    clenv_unify CUMUL ~flags
      (meta_reducible_instance clenv.env clenv.evd clenv.templtyp) concl clenv

let adjust_meta_source evd mv = function
  | loc,Evar_kinds.VarInstance id ->
    let rec match_name c l =
      match EConstr.kind evd c, l with
      | Lambda ({binder_name=Name id},_,c), a::l when EConstr.eq_constr evd a (mkMeta mv) -> Some id
      | Lambda (_,_,c), a::l -> match_name c l
      | _ -> None in
    (* This is very ad hoc code so that an evar inherits the name of the binder
       in situations like "ex_intro (fun x => P) ?ev p" *)
    let f = function (mv',(Cltyp (_,t) | Clval (_,_,t))) ->
      if Metaset.mem mv t.freemetas then
        let f,l = decompose_app evd t.rebus in
        match EConstr.kind evd f with
        | Meta mv'' ->
          (match meta_opt_fvalue evd mv'' with
          | Some (c,_) -> match_name c.rebus l
          | None -> None)
        | _ -> None
      else None in
    let id = try List.find_map f (Evd.Metamap.bindings (Evd.meta_list evd)) with Not_found -> id in
    loc,Evar_kinds.VarInstance id
  | src -> src

(* [clenv_pose_metas_as_evars clenv dep_mvs]
 * For each dependent evar in the clause-env which does not have a value,
 * pose a value for it by constructing a fresh evar.  We do this in
 * left-to-right order, so that every evar's type is always closed w.r.t.
 * metas.

 * Node added 14/4/08 [HH]: before this date, evars were collected in
   clenv_dependent by collect_metas in the fold_constr order which is
   (almost) the left-to-right order of dependencies in term. However,
   due to K-redexes, collect_metas was sometimes missing some metas.
   The call to collect_metas has been replaced by a call to
   undefined_metas, but then the order was the one of definition of
   the metas (numbers in increasing order) which is _not_ the
   dependency order when a clenv_fchain occurs (because clenv_fchain
   plugs a term with a list of consecutive metas in place of a - a priori -
   arbitrary metavariable belonging to another sequence of consecutive metas:
   e.g., clenv_fchain may plug (H ?1 ?2) at the position ?6 of
   (nat_ind ?3 ?4 ?5 ?6), leading to a dependency order 3<4<5<1<2).
   To ensure the dependency order, we check that the type of each meta
   to pose is already meta-free, otherwise we postpone the transformation,
   hoping that no cycle may happen.

   Another approach could have been to use decimal numbers for metas so that
   in the example above, (H ?1 ?2) would have been renumbered (H ?6.1 ?6.2)
   then making the numeric order match the dependency order.
*)

let clenv_pose_metas_as_evars clenv dep_mvs =
  let rec fold clenv = function
  | [] -> clenv
  | mv::mvs ->
      let ty = clenv_meta_type clenv mv in
      (* Postpone the evar-ization if dependent on another meta *)
      (* This assumes no cycle in the dependencies - is it correct ? *)
      if occur_meta clenv.evd ty then fold clenv (mvs@[mv])
      else
        let src = evar_source_of_meta mv clenv.evd in
        let src = adjust_meta_source clenv.evd mv src in
        let evd = clenv.evd in
        let (evd, evar) = new_evar (cl_env clenv) evd ~src ty in
        let clenv = clenv_assign mv evar {clenv with evd=evd} in
        fold clenv mvs in
  fold clenv dep_mvs

(******************************************************************)

(* [clenv_fchain mv clenv clenv']
 *
 * Resolves the value of "mv" (which must be undefined) in clenv to be
 * the template of clenv' be the value "c", applied to "n" fresh
 * metavars, whose types are chosen by destructing "clf", which should
 * be a clausale forme generated from the type of "c".  The process of
 * resolution can cause unification of already-existing metavars, and
 * of the fresh ones which get created.  This operation is a composite
 * of operations which pose new metavars, perform unification on
 * terms, and make bindings.

   Otherwise said, from

     [clenv] = [env;sigma;metas |- c:T]
     [clenv'] = [env';sigma';metas' |- d:U]
     [mv] = [mi] of type [Ti] in [metas]

   then, if the unification of [Ti] and [U] produces map [rho], the
   chaining is [env';sigma';rho'(metas),rho(metas') |- c:rho'(T)] for
   [rho'] being [rho;mi:=d].

   In particular, it assumes that [env'] and [sigma'] extend [env] and [sigma].
*)

let fchain_flags () =
  { (default_unify_flags ()) with
    allow_K_in_toplevel_higher_order_unification = true }

let clenv_instantiate ?(flags=fchain_flags ()) mv clenv (c, ty) =
  (* unify the type of the template of [nextclenv] with the type of [mv] *)
  let clenv = clenv_unify ~flags CUMUL ty (clenv_meta_type clenv mv) clenv in
  clenv_assign mv c clenv

(***************************************************************)
(* Bindings *)

(* [clenv_independent clenv]
 * returns a list of metavariables which appear in the term cval,
 * and which are not dependent.  That is, they do not appear in
 * the types of other metavars which are in cval, nor in the type
 * of cval, ctyp. *)

let clenv_independent clenv =
  let mvs = collect_metas clenv.evd (clenv_value clenv) in
  let ctyp_mvs = (mk_freelisted (clenv_type clenv)).freemetas in
  let deps = Metaset.union (dependent_in_type_of_metas clenv mvs) ctyp_mvs in
  List.filter (fun mv -> not (Metaset.mem mv deps)) mvs

let qhyp_eq h1 h2 = match h1, h2 with
| NamedHyp n1, NamedHyp n2 -> lident_eq n1 n2
| AnonHyp i1, AnonHyp i2 -> Int.equal i1 i2
| _ -> false

let check_bindings bl =
  match List.duplicates qhyp_eq (List.map (fun {CAst.v=x} -> fst x) bl) with
    | NamedHyp s :: _ ->
        user_err ?loc:s.CAst.loc
          Pp.(str "The variable " ++ Id.print s.CAst.v ++
              str " occurs more than once in binding list.");
    | AnonHyp n :: _ ->
        user_err
          Pp.(str "The position " ++ int n ++
              str " occurs more than once in binding list.")
    | [] -> ()

let explain_no_such_bound_variable mvl {CAst.v=id;loc} =
  let open Pp in
  let expl = match mvl with
  | [] -> str "(no bound variables at all in the expression)."
  | [id] -> str "(possible name is: " ++ Id.print id ++ str ")."
  | _ -> str "(possible names are: " ++ pr_enum Id.print mvl ++ str ")."
  in
  user_err ?loc (str "No such bound variable " ++ Id.print id ++ spc () ++ expl)

let meta_with_name evd ({CAst.v=id} as lid) =
  let na = Name id in
  let fold n clb (l1, l2 as l) =
    let (na',def) = match clb with
    | Cltyp (na, _) -> (na, false)
    | Clval (na, _, _) -> (na, true)
    in
    if Name.equal na na' then if def then (n::l1,l2) else (n::l1,n::l2)
    else l
  in
  let (mvl, mvnodef) = Evd.Metamap.fold fold (Evd.meta_list evd) ([], []) in
  match List.rev mvnodef, List.rev mvl with
    | _,[]  ->
      let fold n clb l =
        let na = match clb with
          | Cltyp (na, _) -> na
          | Clval (na, _, _) -> na
        in
        if na != Anonymous then Name.get_id na :: l else l
      in
      let mvl = Evd.Metamap.fold fold (Evd.meta_list evd) [] in
      explain_no_such_bound_variable mvl lid
    | (n::_,_|_,n::_) ->
        n

let meta_of_binder clause loc mvs = function
  | NamedHyp s -> meta_with_name clause.evd s
  | AnonHyp n ->
      try List.nth mvs (n-1)
      with (Failure _|Invalid_argument _) ->
        user_err Pp.(str "No such binder.")

let error_already_defined b =
  match b with
    | NamedHyp id ->
        user_err ?loc:id.CAst.loc
          Pp.(str "Binder name \"" ++ Id.print id.CAst.v ++
              str"\" already defined with incompatible value.")
    | AnonHyp n ->
        anomaly
          Pp.(str "Position " ++ int n ++ str" already defined.")

let clenv_unify_binding_type clenv c t u =
  if isMeta clenv.evd (fst (decompose_app_vect clenv.evd (whd_nored clenv.env clenv.evd u))) then
    (* Not enough information to know if some subtyping is needed *)
    CoerceToType, clenv, c
  else
    (* Enough information so as to try a coercion now *)
    try
      let evd,c = w_coerce_to_type (cl_env clenv) clenv.evd c t u in
      TypeProcessed, { clenv with evd = evd }, c
    with
    | CoqError (PretypeError,
                (_,_, ActualTypeNotCoercible
                   (_,_, (NotClean _ | ConversionFailed _))))
      as e ->
      raise e
    | e when precatchable_exception e ->
      TypeNotProcessed, clenv, c

let clenv_assign_binding clenv k c =
  let k_typ = clenv_hnf_constr clenv (clenv_meta_type clenv k) in
  let c_typ = nf_betaiota clenv.env clenv.evd (clenv_get_type_of clenv c) in
  let status,clenv',c = clenv_unify_binding_type clenv c c_typ k_typ in
  update_clenv_evd clenv' (meta_assign k (c,(Conv,status)) clenv'.evd)

let clenv_match_args bl clenv =
  if List.is_empty bl then
    clenv
  else
    let mvs = clenv_independent clenv in
    check_bindings bl;
    List.fold_left
      (fun clenv {CAst.loc;v=(b,c)} ->
        let k = meta_of_binder clenv loc mvs b in
        match meta_opt_fvalue clenv.evd k with
        | Some (body, _) ->
          if EConstr.eq_constr clenv.evd body.rebus c then clenv
          else error_already_defined b
        | None ->
          clenv_assign_binding clenv k c)
      clenv bl

let error_not_right_number_missing_arguments n =
  user_err
    Pp.(strbrk "Not the right number of missing arguments (expected " ++
        int n ++ str ").")

let clenv_constrain_dep_args hyps_only bl clenv =
  if List.is_empty bl then
    clenv
  else
    let occlist = clenv_dependent_gen hyps_only clenv in
    if Int.equal (List.length occlist) (List.length bl) then
      List.fold_left2 clenv_assign_binding clenv occlist bl
    else
      if hyps_only then
        (* Tolerance for compatibility <= 8.3 *)
        let occlist' = clenv_dependent_gen hyps_only ~iter:false clenv in
        if Int.equal (List.length occlist') (List.length bl) then
          List.fold_left2 clenv_assign_binding clenv occlist' bl
        else
          error_not_right_number_missing_arguments (List.length occlist)
      else
        error_not_right_number_missing_arguments (List.length occlist)


(* This function put casts around metavariables whose type could not be
 * inferred by the refiner, that is head of applications, predicates and
 * subject of Cases.
 * Does check that the casted type is closed. Anyway, the refiner would
 * fail in this case... *)

let clenv_cast_meta clenv =
  let rec crec u =
    match EConstr.kind clenv.evd u with
      | App _ | Case _ -> crec_hd u
      | Cast (c,_,_) when isMeta clenv.evd c -> u
      | Proj (p, c) -> mkProj (p, crec_hd c)
      | _  -> EConstr.map clenv.evd crec u

  and crec_hd u =
    match EConstr.kind clenv.evd (strip_outer_cast clenv.evd u) with
      | Meta mv ->
          (try
            let b = Typing.meta_type clenv.env clenv.evd mv in
            assert (not (occur_meta clenv.evd b));
            if occur_meta clenv.evd b then u
            else mkCast (mkMeta mv, DEFAULTcast, b)
          with Not_found -> u)
      | App(f,args) -> mkApp (crec_hd f, Array.map crec args)
      | Case(ci,u,pms,p,iv,c,br) ->
          (* FIXME: we only change c because [p] is always a lambda and [br] is
             most of the time??? *)
          mkCase (ci, u, pms, p, iv, crec_hd c, br)
      | Proj (p, c) -> mkProj (p, crec_hd c)
      | _ -> u
  in
  crec

let clenv_pose_dependent_evars ?(with_evars=false) clenv =
  let dep_mvs = clenv_dependent clenv in
  let env, sigma = clenv.env, clenv.evd in
  if not (List.is_empty dep_mvs) && not with_evars then
    CErrors.coq_error RefinerError
      (env, sigma, UnresolvedBindings (List.map (meta_name clenv.evd) dep_mvs));
  clenv_pose_metas_as_evars clenv dep_mvs

module Internal =
struct

open Pp
open Constr
open CVars
open Termops
open Retyping

let error_unsupported_deep_meta c =
  user_err  (strbrk "Application of lemmas whose beta-iota normal " ++
    strbrk "form contains metavariables deep inside the term is not " ++
    strbrk "supported; try \"refine\" instead.")

let collect_meta_variables c =
  let rec collrec deep acc c = match kind c with
    | Meta mv -> if deep then error_unsupported_deep_meta () else mv::acc
    | Cast(c,_,_) -> collrec deep acc c
    | Case(ci,u,pms,p,iv,c,br) ->
      let acc = Array.fold_left (collrec deep) acc pms in
      let acc = Constr.fold (collrec deep) acc (snd p) in
      let acc = Constr.fold_invert (collrec deep) acc iv in
      let acc = Constr.fold (collrec deep) acc c in
      Array.fold_left (fun accu (_, br) -> collrec deep accu br) acc br
    | App _ -> Constr.fold (collrec deep) acc c
    | Proj (_, c) -> collrec deep acc c
    | _ -> Constr.fold (collrec true) acc c
  in
  List.rev (collrec false [] c)

let check_meta_variables env sigma c =
  if not (List.distinct_f Int.compare (collect_meta_variables c)) then
    CErrors.coq_error RefinerError (env, sigma, NonLinearProof c)

exception Stop of EConstr.t list
let meta_free_prefix sigma a =
  try
    let a = Array.map EConstr.of_constr a in
    let _ = Array.fold_left (fun acc a ->
      if occur_meta sigma a then raise (Stop acc)
      else a :: acc) [] a
    in a
  with Stop acc -> Array.rev_of_list acc

let goal_type_of env sigma c =
  (sigma, EConstr.Unsafe.to_constr (Retyping.get_type_of env sigma (EConstr.of_constr c)))

(* Old style mk_goal primitive *)
let mk_goal evars hyps concl =
  (* A goal created that way will not be used by refine and will not
      be shelved. It must not appear as a future_goal, so the future
      goals are restored to their initial value after the evar is
      created. *)
  let evars = Evd.push_future_goals evars in
  let inst = EConstr.identity_subst_val hyps in
  let (evars,evk) =
    Evarutil.new_pure_evar ~src:(Loc.tag Evar_kinds.GoalEvar) ~typeclass_candidate:false hyps evars concl
  in
  let _, evars = Evd.pop_future_goals evars in
  let ev = EConstr.mkEvar (evk,inst) in
  (evk, ev, evars)

let rec mk_refgoals env sigma goalacc conclty trm =
  let hyps = Environ.named_context_val env in
    if not (occur_meta sigma (EConstr.of_constr trm)) then
      let t'ty = Retyping.get_type_of env sigma (EConstr.of_constr trm) in
      let t'ty = EConstr.Unsafe.to_constr t'ty in
        (goalacc,t'ty,sigma,trm)
    else
      match kind trm with
      | Meta _ ->
        let conclty = nf_betaiota env sigma conclty in
          let (gl,ev,sigma) = mk_goal sigma hyps conclty in
          let ev = EConstr.Unsafe.to_constr ev in
          let conclty = EConstr.Unsafe.to_constr conclty in
          gl::goalacc, conclty, sigma, ev

      | Cast (t,k, ty) ->
        let res = mk_refgoals env sigma goalacc (EConstr.of_constr ty) t in
        (* we keep the casts (in particular VMcast and NATIVEcast) except
           when they are annotating metas *)
        if isMeta t then begin
          assert (k != VMcast && k != NATIVEcast);
          res
        end else
          let (gls,cty,sigma,ans) = res in
          let ans = if ans == t then trm else mkCast(ans,k,ty) in
          (gls,cty,sigma,ans)

      | App (f,l) ->
        let (acc',hdty,sigma,applicand) =
          if Termops.is_template_polymorphic_ind env sigma (EConstr.of_constr f) then
            let ty =
              (* Template polymorphism of definitions and inductive types *)
              let firstmeta = Array.findi (fun i x -> occur_meta sigma (EConstr.of_constr x)) l in
              let args, _ = Option.cata (fun i -> CArray.chop i l) (l, [||]) firstmeta in
                type_of_global_reference_knowing_parameters env sigma (EConstr.of_constr f) (Array.map EConstr.of_constr args)
            in
            let ty = EConstr.Unsafe.to_constr ty in
              goalacc, ty, sigma, f
          else
            mk_hdgoals env sigma goalacc f
        in
        let ((acc'',conclty',sigma), args) = mk_arggoals env sigma acc' hdty l in
        let ans = if applicand == f && args == l then trm else mkApp (applicand, args) in
        (acc'',conclty',sigma, ans)

      | Proj (p,c) ->
        let (acc',cty,sigma,c') = mk_hdgoals env sigma goalacc c in
        let c = mkProj (p, c') in
        let ty = get_type_of env sigma (EConstr.of_constr c) in
        let ty = EConstr.Unsafe.to_constr ty in
          (acc',ty,sigma,c)

      | Case (ci, u, pms, p, iv, c, lf) ->
        (* XXX Is ignoring iv OK? *)
        let (ci, p, iv, c, lf) = Inductive.expand_case env (ci, u, pms, p, iv, c, lf) in
        let (acc',lbrty,conclty',sigma,p',c') = mk_casegoals env sigma goalacc p c in
        let (acc'',sigma,rbranches) = treat_case env sigma ci lbrty lf acc' in
        let lf' = Array.rev_of_list rbranches in
        let ans =
          if p' == p && c' == c && Array.equal (==) lf' lf then trm
          else mkCase (Inductive.contract_case env (ci,p',iv,c',lf'))
        in
        (acc'',conclty',sigma, ans)

      | _ ->
        if occur_meta sigma (EConstr.of_constr trm) then
          anomaly (Pp.str "refiner called with a meta in non app/case subterm.");
        let (sigma, t'ty) = goal_type_of env sigma trm in
          (goalacc,t'ty,sigma, trm)

(* Same as mkREFGOALS but without knowing the type of the term. Therefore,
 * Metas should be casted. *)

and mk_hdgoals env sigma goalacc trm =
  let hyps = Environ.named_context_val env in
  match kind trm with
    | Cast (c,_, ty) when isMeta c ->
        let (gl,ev,sigma) = mk_goal sigma hyps (nf_betaiota env sigma (EConstr.of_constr ty)) in
        let ev = EConstr.Unsafe.to_constr ev in
        gl::goalacc,ty,sigma,ev

    | Cast (t,_, ty) ->
        mk_refgoals env sigma goalacc (EConstr.of_constr ty) t

    | App (f,l) ->
        let (acc',hdty,sigma,applicand) =
          if Termops.is_template_polymorphic_ind env sigma (EConstr.of_constr f)
          then
            let l' = meta_free_prefix sigma l in
           (goalacc,EConstr.Unsafe.to_constr (type_of_global_reference_knowing_parameters env sigma (EConstr.of_constr f) l'),sigma,f)
          else mk_hdgoals env sigma goalacc f
        in
        let ((acc'',conclty',sigma), args) = mk_arggoals env sigma acc' hdty l in
        let ans = if applicand == f && args == l then trm else mkApp (applicand, args) in
        (acc'',conclty',sigma, ans)

    | Case (ci, u, pms, p, iv, c, lf) ->
        (* XXX is ignoring iv OK? *)
        let (ci, p, iv, c, lf) = Inductive.expand_case env (ci, u, pms, p, iv, c, lf) in
        let (acc',lbrty,conclty',sigma,p',c') = mk_casegoals env sigma goalacc p c in
        let (acc'',sigma,rbranches) = treat_case env sigma ci lbrty lf acc' in
        let lf' = Array.rev_of_list rbranches in
        let ans =
          if p' == p && c' == c && Array.equal (==) lf' lf then trm
          else mkCase (Inductive.contract_case env (ci,p',iv,c',lf'))
        in
        (acc'',conclty',sigma, ans)

    | Proj (p,c) ->
         let (acc',cty,sigma,c') = mk_hdgoals env sigma goalacc c in
         let c = mkProj (p, c') in
         let ty = get_type_of env sigma (EConstr.of_constr c) in
         let ty = EConstr.Unsafe.to_constr ty in
           (acc',ty,sigma,c)

    | _ ->
        let (sigma, ty) = goal_type_of env sigma trm in
        goalacc, ty, sigma, trm

and mk_arggoals env sigma goalacc funty allargs =
  let foldmap (goalacc, funty, sigma) harg =
    let t = whd_all env sigma (EConstr.of_constr funty) in
    let t = EConstr.Unsafe.to_constr t in
    let rec collapse t = match kind t with
    | LetIn (_, c1, _, b) -> collapse (subst1 c1 b)
    | _ -> t
    in
    let t = collapse t in
    match kind t with
    | Prod (_, c1, b) ->
      let (acc, hargty, sigma, arg) = mk_refgoals env sigma goalacc (EConstr.of_constr c1) harg in
      (acc, subst1 harg b, sigma), arg
    | _ ->
      CErrors.coq_error RefinerError (env,sigma,CannotApply (t, harg))
  in
  Array.Smart.fold_left_map foldmap (goalacc, funty, sigma) allargs

and mk_casegoals env sigma goalacc p c =
  let (acc',ct,sigma,c') = mk_hdgoals env sigma goalacc c in
  let ct = EConstr.of_constr ct in
  let (acc'',pt,sigma,p') = mk_hdgoals env sigma acc' p in
  let ((ind, u), spec) =
    try Tacred.find_hnf_rectype env sigma ct
    with Not_found -> anomaly (Pp.str "mk_casegoals.") in
  let indspec = ((ind, EConstr.EInstance.kind sigma u), spec) in
  let (lbrty,conclty) = Inductiveops.type_case_branches_with_names env sigma indspec p c in
  (acc'',lbrty,conclty,sigma,p',c')

and treat_case env sigma ci lbrty lf acc' =
  let rec strip_outer_cast c = match kind c with
  | Cast (c,_,_) -> strip_outer_cast c
  | _ -> c in
  let decompose_app_vect c = match kind c with
  | App (f,cl) -> (f, cl)
  | _ -> (c,[||]) in
  Array.fold_left3
    (fun (lacc,sigma,bacc) ty fi n ->
        (* Deal with a branch in expanded form of the form
           Case(ci,p,c,[|eta-let-exp(Meta);...;eta-let-exp(Meta)|]) as
           if it were not so, so as to preserve compatibility with when
           destruct/case generated schemes of the form
           Case(ci,p,c,[|Meta;...;Meta|];
           CAUTION: it does not deal with the general case of eta-zeta
           reduced branches having a form different from Meta, as it
           would be theoretically the case with third-party code *)
        let ctx, body = Term.decompose_lambda_n_decls n fi in
        let head, args = decompose_app_vect body in
        (* Strip cast because clenv_cast_meta adds a cast when the branch is
           eta-expanded but when not when the branch has the single-meta
           form [Meta] *)
        let head = strip_outer_cast head in
        if isMeta head then begin
          assert (args = Context.Rel.instance mkRel 0 ctx);
          let (r,_,s,head'') = mk_refgoals env sigma lacc ty head in
          let fi' = Term.it_mkLambda_or_LetIn (mkApp (head'',args)) ctx in
          (r,s,fi'::bacc)
        end
        else
          (* Supposed to be meta-free *)
          let sigma, t'ty = goal_type_of env sigma fi in
          (lacc,sigma,fi::bacc))
    (acc',sigma,[]) lbrty lf ci.ci_cstr_ndecls

let refiner clenv =
  let open Proofview.Notations in
  let r = EConstr.Unsafe.to_constr (clenv_cast_meta clenv @@ clenv_value clenv) in
  Proofview.Goal.enter begin fun gl ->
  let sigma = Proofview.Goal.sigma gl in
  let env = Proofview.Goal.env gl in
  let st = Proofview.Goal.state gl in
  let cl = Proofview.Goal.concl gl in
  check_meta_variables env sigma r;
  let (sgl,cl',sigma,oterm) = mk_refgoals env sigma [] cl r in
  let map gl = Proofview.goal_with_state gl st in
  let sgl = List.rev_map map sgl in
  let evk = Proofview.Goal.goal gl in
  let c = EConstr.of_constr oterm in
  (* Check that the goal itself does not appear in the refined term *)
  let _ =
    if not (Evarutil.occur_evar_upto sigma evk c) then ()
    else Pretype_errors.error_occur_check env sigma evk c
  in
  let sigma = Evd.define evk c sigma in
  Proofview.Unsafe.tclEVARS sigma <*>
  Proofview.Unsafe.tclSETGOALS sgl
  end
end

open Unification

let dft = default_unify_flags

let res_pf ?(with_evars=false) ?(with_classes=true) ?(flags=dft ()) clenv =
  Proofview.Goal.enter begin fun gl ->
    let concl = Proofview.Goal.concl gl in
    let clenv = clenv_unique_resolver ~flags clenv concl in
    let clenv = clenv_pose_dependent_evars ~with_evars clenv in
    let evd' =
      if with_classes then
        let evd' =
          Typeclasses.resolve_typeclasses ~filter:Typeclasses.all_evars
            ~fail:(not with_evars) clenv.env clenv.evd
        in
        (* After an apply, all the subgoals including those dependent shelved ones are in
          the hands of the user and resolution won't be called implicitely on them. *)
        Typeclasses.make_unresolvables (fun x -> true) evd'
      else clenv.evd
    in
    let clenv = update_clenv_evd clenv evd' in
    Proofview.tclTHEN
      (Proofview.Unsafe.tclEVARS (Evd.clear_metas evd'))
      (Internal.refiner clenv)
  end

(* [unifyTerms] et [unify] ne semble pas gérer les Meta, en
   particulier ne semblent pas vérifier que des instances différentes
   d'une même Meta sont compatibles. D'ailleurs le "fst" jette les metas
   provenant de w_Unify. (Utilisé seulement dans prolog.ml) *)

let fail_quick_core_unif_flags = {
  modulo_conv_on_closed_terms = Some TransparentState.full;
  use_metas_eagerly_in_conv_on_closed_terms = false;
  use_evars_eagerly_in_conv_on_closed_terms = false;
  modulo_delta = TransparentState.empty;
  modulo_delta_types = TransparentState.full;
  check_applied_meta_types = false;
  use_pattern_unification = false;
  use_meta_bound_pattern_unification = true; (* ? *)
  allowed_evars = Evarsolve.AllowedEvars.all;
  restrict_conv_on_strict_subterms = false; (* ? *)
  modulo_betaiota = false;
  modulo_eta = true;
}

let fail_quick_unif_flags = {
  core_unify_flags = fail_quick_core_unif_flags;
  merge_unify_flags = fail_quick_core_unif_flags;
  subterm_unify_flags = fail_quick_core_unif_flags;
  allow_K_in_toplevel_higher_order_unification = false;
  resolve_evars = false
}

(* let unifyTerms m n = walking (fun wc -> fst (w_Unify CONV m n [] wc)) *)
let unify ?(flags=fail_quick_unif_flags) m =
  Proofview.Goal.enter begin fun gl ->
    let env = Tacmach.pf_env gl in
    let n = Tacmach.pf_concl gl in
    let evd = clear_metas (Tacmach.project gl) in
    try
      let evd' = w_unify env evd CONV ~flags m n in
        Proofview.Unsafe.tclEVARSADVANCE evd'
    with e when CErrors.noncritical e ->
      let info = Exninfo.reify () in
      Proofview.tclZERO ~info e
  end

(****************************************************************)
(* Clausal environment for an application *)

let make_clenv_binding_gen hyps_only n env sigma (c,t) = function
  | ImplicitBindings largs ->
      let clause = mk_clenv_from_env env sigma n (c,t) in
      clenv_constrain_dep_args hyps_only largs clause
  | ExplicitBindings lbind ->
      let clause = mk_clenv_from_env env sigma n (c, t) in clenv_match_args lbind clause
  | NoBindings ->
      mk_clenv_from_env env sigma n (c,t)

let make_clenv_binding_apply env sigma n = make_clenv_binding_gen true n env sigma
let make_clenv_binding env sigma = make_clenv_binding_gen false None env sigma

(****************************************************************)
(* Pretty-print *)

let pr_clenv clenv =
  let prc = Termops.Internal.print_constr_env clenv.env clenv.evd in
  Pp.(h (str"TEMPL: " ++ prc clenv.templval.rebus ++
         str" : " ++ prc clenv.templtyp.rebus ++ fnl () ++
         pr_evar_map (Some 2) clenv.env clenv.evd))

(****************************************************************)
(** Evar version of mk_clenv *)

type hole = {
  hole_evar : EConstr.constr;
  hole_type : EConstr.types;
  hole_deps  : bool;
  hole_name : Name.t;
}

type clause = {
  cl_holes : hole list;
  cl_concl : EConstr.types;
}

let make_evar_clause env sigma ?len t =
  let open EConstr in
  let open Vars in
  let bound = match len with
  | None -> -1
  | Some n -> assert (0 <= n); n
  in
  let rec clrec (sigma, holes) inst n t =
    if n = 0 then (sigma, holes, t)
    else match EConstr.kind sigma t with
    | Cast (t, _, _) -> clrec (sigma, holes) inst n t
    | Prod (na, t1, t2) ->
      (* Share the evar instances as we are living in the same context *)
      let inst, ctx, args, subst = match inst with
      | None ->
        (* Dummy type *)
        let hypnaming = RenameExistingBut (VarSet.variables (Global.env ())) in
        let ctx, _, args, subst = push_rel_context_to_named_context ~hypnaming env sigma mkProp in
        Some (ctx, args, subst), ctx, args, subst
      | Some (ctx, args, subst) -> inst, ctx, args, subst
      in
      let (sigma, ev) = new_pure_evar ~typeclass_candidate:false ctx sigma (csubst_subst sigma subst t1) in
      let ev = mkEvar (ev, args) in
      let dep = not (noccurn sigma 1 t2) in
      let hole = {
        hole_evar = ev;
        hole_type = t1;
        hole_deps = dep;
        (* We fix it later *)
        hole_name = na.binder_name;
      } in
      let t2 = subst1 ev t2 in
      clrec (sigma, hole :: holes) inst (pred n) t2
    | LetIn (na, b, _, t) -> clrec (sigma, holes) inst n (subst1 b t)
    | _ -> (sigma, holes, t)
  in
  let (sigma, holes, t) = clrec (sigma, []) None bound t in
  let holes = List.rev holes in
  let clause = { cl_concl = t; cl_holes = holes } in
  (sigma, clause)

let evar_with_name holes ({CAst.v=id} as lid) =
  let map h = match h.hole_name with
  | Anonymous -> None
  | Name id' -> if Id.equal id id' then Some h else None
  in
  let hole = List.map_filter map holes in
  match hole with
  | [] ->
    let fold h accu = match h.hole_name with
      | Anonymous -> accu
      | Name id -> id :: accu
    in
    let mvl = List.fold_right fold holes [] in
    explain_no_such_bound_variable mvl lid
  | h::_ -> h.hole_evar

let evar_of_binder holes = function
| NamedHyp s -> evar_with_name holes s
| AnonHyp n ->
  try
    let nondeps = List.filter (fun hole -> not hole.hole_deps) holes in
    let h = List.nth nondeps (pred n) in
    h.hole_evar
  with e when CErrors.noncritical e ->
    user_err Pp.(str "No such binder.")

let define_with_type sigma env ev c =
  let open EConstr in
  let t = Retyping.get_type_of env sigma ev in
  let ty = Retyping.get_type_of env sigma c in
  let j = Environ.make_judge c ty in
  let (sigma, j, _trace) = Coercion.inh_conv_coerce_to ~program_mode:false ~resolve_tc:true env sigma j t in
  let (ev, _) = destEvar sigma ev in
  let sigma = Evd.define ev j.Environ.uj_val sigma in
  sigma

let solve_evar_clause env sigma hyp_only clause = function
| NoBindings -> sigma
| ImplicitBindings largs ->
  let open EConstr in
  let fold holes h =
    if h.hole_deps then
      (* Some subsequent term uses the hole *)
      let (ev, _) = destEvar sigma h.hole_evar in
      let is_dep hole = occur_evar sigma ev hole.hole_type in
      let in_hyp = List.exists is_dep holes in
      let in_ccl = occur_evar sigma ev clause.cl_concl in
      let dep = if hyp_only then in_hyp && not in_ccl else in_hyp || in_ccl in
      let h = { h with hole_deps = dep } in
      h :: holes
    else
      (* The hole does not occur anywhere *)
      h :: holes
  in
  let holes = List.fold_left fold [] (List.rev clause.cl_holes) in
  let map h = if h.hole_deps then Some h.hole_evar else None in
  let evs = List.map_filter map holes in
  let len = List.length evs in
  if Int.equal len (List.length largs) then
    let fold sigma ev arg = define_with_type sigma env ev arg in
    let sigma = List.fold_left2 fold sigma evs largs in
    sigma
  else
    error_not_right_number_missing_arguments len
| ExplicitBindings lbind ->
  let () = check_bindings lbind in
  let fold sigma {CAst.v=(binder, c)} =
    let ev = evar_of_binder clause.cl_holes binder in
    define_with_type sigma env ev c
  in
  let sigma = List.fold_left fold sigma lbind in
  sigma
