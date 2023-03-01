(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2019       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Names
open Univ
module RelDecl = Context.Rel.Declaration

let __ () = assert false

let invalid = Constr.(mkApp (mkSet, [| mkSet |]))

let add_universe l ~lbound g =
  let g = UGraph.add_universe l ~lbound:Set ~strict:false g in
  UGraph.enforce_constraint (lbound, Le, l) g

let quickdef ~name ~types ~univs body =
  let entry = Declare.definition_entry ?types ~univs body in
  let scope = Locality.(Global ImportDefaultBehavior) in
  let kind = Decls.(IsDefinition Definition) in
  let uctx = UState.empty (* used for ubinders and hook *) in
  Declare.declare_entry ~name ~scope ~kind ~impargs:[] ~uctx entry

type extended_level =
  | Level of Level.t
  | LSProp

(** produce [args, recargs] inside mixed context [args/recargs]
    [info] is in reverse order, ie the head is about the last arg in application order

    examples:
    [] -> [],[]
    [false] -> [1],[]
    [true] -> [2],[1]
    [true;false] -> [3;2],[1]
    [false;true] -> [3;1],[2]
    [false;false] -> [2;1],[]
    [true;true] -> [4;2],[3;1]

*)
let reorder_inside_core info =
  let rec aux i args recargs = function
    | [] -> (args, recargs)
    | false :: info -> aux (i + 1) (i :: args) recargs info
    | true :: info -> aux (i + 2) ((i + 1) :: args) (i :: recargs) info
  in
  aux 1 [] [] info

let reorder_inside info =
  let args, recargs = reorder_inside_core info in
  CArray.map_of_list Constr.mkRel (List.append args recargs)

let rec insert_after k ((n, t) as v) c =
  if k = 0 then Constr.mkProd (n, t, Vars.lift 1 c)
  else
    match Constr.kind c with
    | Prod (na, a, b) -> Constr.mkProd (na, a, insert_after (k - 1) v b)
    | _ -> assert false

let insert_after k (n, t) c =
  insert_after k (n, Vars.lift k t) (Vars.subst1 invalid c)

let rec reorder_outside info hyps c =
  match (info, hyps) with
  | [], [] -> (c, 0)
  | false :: info, (n, t) :: hyps ->
    let c, k = reorder_outside info hyps c in
    (Constr.mkProd (n, t, c), k + 1)
  | true :: info, (n, t) :: rhyp :: hyps ->
    let c, k = reorder_outside info hyps c in
    let c = insert_after k rhyp c in
    (Constr.mkProd (n, t, c), k + 1)
  | _ -> assert false

(** produce [forall args recargs, P (C args)] from mixed [forall args/recargs, P (C args)] *)
let reorder_outside info ft =
  let hyps, out = Term.decompose_prod ft in
  fst (reorder_outside (List.rev info) (List.rev hyps) out)

exception Occur
let occur_mind mind term =
  let rec occur_rec c = match Constr.kind c with
    | Constr.Ind ((mind',_),_) -> if MutInd.UserOrd.equal mind mind' then raise_notrace Occur
    | _ -> Constr.iter occur_rec c
  in
  try occur_rec term; true with Occur -> false

(** Build the body of a Lean-style scheme. [u] instantiates the
    inductive, [s] is [None] for the SProp scheme and [Some l]
    for a scheme with motive [l].

    Lean schemes differ from Coq schemes:
    - the motive universe is the first bound universe for Lean
      but the last for Coq (handled by caller)
    - Lean puts induction hypotheses after all the constructor arguments,
      Coq puts them immediately after the corresponding recursive argument.
 *)
let lean_scheme env ~dep mind u s =
  let mib = Global.lookup_mind mind in
  let nparams = mib.mind_nparams in
  assert (Array.length mib.mind_packets = 1);
  (* if we start using non recursive params in the translation it will
     involve reordering arguments *)
  assert (nparams = mib.mind_nparams_rec);
  let mip = mib.mind_packets.(0) in

  let body =
    let sigma = Evd.from_env env in
    let sigma, body =
      Indrec.build_induction_scheme env sigma
        ((mind, 0), u)
        dep
        (if s = LSProp then InSProp else InType)
    in
    let uctx = Evd.universe_context_set sigma in
    match s with
    | LSProp ->
      assert (ContextSet.is_empty uctx);
      body
    | Level s ->
      assert (Level.Set.cardinal (fst uctx) = 1 && Constraints.is_empty (snd uctx));
      let v = Level.Set.choose (fst uctx) in
      Vars.subst_univs_level_constr (Level.Map.singleton v s) body
  in

  assert (
    CArray.for_all2 Int.equal mip.mind_consnrealargs mip.mind_consnrealdecls);
  let recinfo =
    Array.mapi
      (fun i (args, _) ->
        let nargs = mip.mind_consnrealargs.(i) in
        (* skip params *)
        let args = CList.firstn nargs args in
        CList.map
          (fun arg ->
             let t = RelDecl.get_type arg in
             not (occur_mind mind t))
          args)
      mip.mind_nf_lc
  in
  let hasrec =
    (* NB: if the only recursive arg is the last arg, no need for reordering *)
    Array.exists
      (function [] -> false | _ :: info -> List.exists (fun x -> x) info)
      recinfo
  in

  if not hasrec then body
  else
    (* body := fun params P (fc : forall args/recargs, P (C args)) => ...

       becomes

       fun params P (fc : forall args, forall recargs, P (C args)) =>
       body params P (fun args/recargs, fc args recargs)
    *)
    let open Constr in
    let nlc = Array.length recinfo in
    let paramsP, inside = Term.decompose_lambda_n_assum (nparams + 1) body in
    let fcs, inside = Term.decompose_lambda_n nlc inside in
    let fcs = List.rev fcs in

    let body =
      mkApp
        (body, Array.init (nparams + 1) (fun i -> mkRel (nlc + nparams + 1 - i)))
    in
    let body =
      mkApp
        ( body,
          Array.of_list
            (CList.map_i
               (fun i (_, ft) ->
                 let info = recinfo.(i) in
                 if not (List.exists (fun x -> x) info) then mkRel (nlc - i)
                 else
                   let hyps, _ = Term.decompose_prod ft in
                   let args = reorder_inside info in
                   Term.it_mkLambda_or_LetIn
                     (mkApp (mkRel (nlc - i + List.length hyps), args))
                     (List.map (fun (n, t) -> RelDecl.LocalAssum (n, t)) hyps))
               0 fcs) )
    in

    let fcs =
      CList.map_i
        (fun i (n, ft) ->
          let info = recinfo.(i) in
          let ft = reorder_outside info ft in
          RelDecl.LocalAssum (n, ft))
        0 fcs
    in
    let fcs = List.rev fcs in
    let body =
      let open CClosure in
      let env = Environ.push_rel_context paramsP env in
      let env = Environ.push_rel_context fcs env in
      norm_val (create_clos_infos betaiota env) (create_tab ()) (inject body)
    in
    Term.it_mkLambda_or_LetIn (Term.it_mkLambda_or_LetIn body fcs) paramsP

let with_unsafe_univs f () =
  let flags = Global.typing_flags () in
  Global.set_typing_flags { flags with check_universes = false };
  try
    let v = f () in
    Global.set_typing_flags flags;
    v
  with e ->
    let e = Exninfo.capture e in
    Global.set_typing_flags flags;
    Exninfo.iraise e

module RRange : sig
  type +'a t
  (** Like Range.t, but instead of cons we append *)

  val empty : 'a t

  val length : 'a t -> int

  val append : 'a t -> 'a -> 'a t

  val get : 'a t -> int -> 'a

  val singleton : 'a -> 'a t
end = struct
  type 'a t = { data : 'a Range.t; len : int }

  let empty = { data = Range.empty; len = 0 }

  let length x = x.len

  let append { data; len } x = { data = Range.cons x data; len = len + 1 }

  let get { data; len } i =
    if i >= len then raise Not_found else Range.get data (len - i - 1)

  let singleton x = { data = Range.cons x Range.empty; len = 1 }
end

module LeanName : sig
  type t = private string list

  val anon : t

  val of_list : string list -> t [@@warning "-32"]

  val append : t -> string -> t

  val equal : t -> t -> bool

  val raw_append : t -> string -> t
  (** for private names *)

  val to_coq_string : t -> string

  val to_lean_string : t -> string

  val to_id : t -> Id.t

  val to_name : t -> Name.t

  val pp : t -> Pp.t

  module Set : CSet.S with type elt = t

  module Map : CMap.ExtS with type key = t and module Set := Set
end = struct
  type t = string list
  (** "foo.bar.baz" is [baz;bar;foo] (like with dirpaths) *)

  let anon : t = []

  let of_list x = x

  let clean_string s = String.concat "__at__" (String.split_on_char '@' s)

  let append a b = clean_string b :: a

  let raw_append a b = match a with [] -> [ b ] | hd :: tl -> (hd ^ b) :: tl

  let to_id (x : t) = Id.of_string (String.concat "_" (List.rev x))

  let to_name x = if x = [] then Anonymous else Name (to_id x)

  let to_coq_string x = String.concat "_" (List.rev x)

  let to_lean_string x = String.concat "." (List.rev x)

  let pp x = Pp.(prlist_with_sep (fun () -> str ".") str) (List.rev x)

  let equal = CList.equal String.equal

  module Self = struct
    type nonrec t = t

    let compare = CList.compare String.compare
  end

  module Set = Set.Make (Self)
  module Map = CMap.Make (Self)
end

module N = LeanName

module U = struct
  type t = Prop | Succ of t | Max of t * t | IMax of t * t | UNamed of N.t
end

let sort_of_level = function
  | LSProp -> Sorts.sprop
  | Level u -> Sorts.sort_of_univ (Universe.make u)

let univ_of_sort = function
  | Sorts.SProp -> None
  | Prop -> assert false
  | Set -> Some Universe.type0
  | Type u | QSort (_, u) -> Some u

let sort_max (s1:Sorts.t) (s2:Sorts.t) = match s1, s2 with
| (SProp, SProp) | (Prop, Prop) | (Set, Set) -> s1
| (SProp, (Prop | Set | Type _ as s)) | ((Prop | Set | Type _) as s, SProp) -> s
| (Prop, (Set | Type _ as s)) | ((Set | Type _) as s, Prop) -> s
| (Set, Type u) | (Type u, Set) -> Sorts.sort_of_univ (Univ.Universe.sup Univ.Universe.type0 u)
| (Type u, Type v) -> Sorts.sort_of_univ (Univ.Universe.sup u v)
| (QSort _, _) | (_, QSort _) -> assert false


(** [map] goes from lean names to universes (in practice either SProp or a named level) *)
let rec to_universe map = function
  | U.Prop -> Sorts.sprop
  | UNamed n -> sort_of_level (N.Map.get n map)
  | Succ u -> Sorts.super (to_universe map u)
  | Max (a, b) -> sort_max (to_universe map a) (to_universe map b)
  | IMax (a, b) ->
    let ub = to_universe map b in
    if Sorts.is_sprop ub then ub else sort_max (to_universe map a) ub

let rec do_n f x n = if n = 0 then x else do_n f (f x) (n - 1)

(** in lean, imax(Prop+1,l)+1 <= max(Prop+1,l+1)
   because:
   - either l=Prop, so Prop+1 <= Prop+1
   - or Prop+1 <= l so l+1 <= l+1

   to simulate this, each named level becomes > Set and we compute maxes accordingly
*)

let simplify_universe u =
  match Universe.repr u with
  | (l, n) :: (l',n') :: rest when Level.is_set l ->
    if n <= n' + 1 || List.exists (fun (_, n') -> n <= n' + 1) rest then
      List.fold_left
        (fun u (l, n) ->
          Universe.sup u (do_n Universe.super (Universe.make l) n))
        (do_n Universe.super (Universe.make l') n') rest
    else u
  | _ -> u

let simplify_sort = function
  | Sorts.Type u -> Sorts.sort_of_univ (simplify_universe u)
  | s -> s

(* Return the biggest [n] such that [Set+n <= u]. Assumes a simplified
   [u] as above. *)
let max_increment u =
  match Universe.repr u with
  | (l, n) :: rest when Level.is_set l -> n
  | l -> List.fold_left (fun m (_, n) -> max m (n + 1)) 0 l

let to_universe map u =
  let u = to_universe map u in
  simplify_sort u

(** Map from [n] to the global level standing for [Set+n] (not including n=0). *)
let sets : Level.t Int.Map.t ref =
  Summary.ref ~name:"lean-set-surrogates" Int.Map.empty

type uconv = {
  map : extended_level N.Map.t;  (** Map from lean names to Coq universes *)
  levels : Level.t Universe.Map.t;
      (** Map from algebraic universes to levels (only levels representing
          an algebraic) *)
  graph : UGraph.t;
}

let lean_id = Id.of_string "Lean"

let lean_fancy_univs =
  Goptions.declare_bool_option_and_ref ~depr:false
    ~stage:Interp
    ~key:[ "Lean"; "Fancy"; "Universes" ]
    ~value:true

let level_of_universe_core u =
  let u =
    List.map
      (fun (l, n) ->
        let open Level in
        if is_set l then "Set+" ^ string_of_int n
        else
          match name l with
          | None -> assert false
          | Some name ->
            let d, s, i = UGlobal.repr name in
            let d = DirPath.repr d in
            (match (d, i) with
            | [ name; l ], 0 when Id.equal l lean_id ->
              Id.to_string name ^ if n = 0 then "" else "+" ^ string_of_int n
            | _ -> to_string l))
      u
  in
  let s = (match u with [ _ ] -> "" | _ -> "max__") ^ String.concat "_" u in
  Level.(make (UGlobal.make (DirPath.make [ Id.of_string_soft s; lean_id ]) "" 0))

let level_of_universe u =
  let u = Universe.repr u in
  level_of_universe_core u

let level_of_universe u =
  if lean_fancy_univs () then level_of_universe u else UnivGen.fresh_level ()

let update_graph (l, u) (l', u') graph =
  if UGraph.check_leq graph (Universe.super u) u' then
    UGraph.enforce_constraint (l, Lt, l') graph
  else if UGraph.check_leq graph (Universe.super u') u then
    UGraph.enforce_constraint (l', Lt, l) graph
  else if UGraph.check_leq graph u u' then
    UGraph.enforce_constraint (l, Le, l') graph
  else if UGraph.check_leq graph u' u then
    UGraph.enforce_constraint (l', Le, l) graph
  else graph

let is_sets u =
  match Universe.repr u with
  | [ (u, n) ] -> if Level.is_set u then Some n else None
  | _ -> None

(** Find or add a global level for Set+n *)
let rec level_of_sets uconv n =
  if n = 0 then (uconv, Level.set)
  else
    try (uconv, Int.Map.find n !sets)
    with Not_found ->
      let uconv, p = level_of_sets uconv (n - 1) in
      let l =
        if lean_fancy_univs () then level_of_universe_core [ (Level.set, n) ]
        else UnivGen.fresh_level ()
      in
      Global.push_context_set ~strict:true
        (Level.Set.singleton l, Constraints.singleton (p, Lt, l));
      sets := Int.Map.add n l !sets;
      let graph = add_universe l ~lbound:p uconv.graph in
      ({ uconv with graph }, l)

let to_univ_level u uconv =
  match Universe.level u with
  | Some l -> (uconv, l)
  | None ->
    (match is_sets u with
    | Some n -> level_of_sets uconv n
    | None ->
      (match Universe.Map.find_opt u uconv.levels with
      | Some l -> (uconv, l)
      | None ->
        let uconv, mset = level_of_sets uconv (max_increment u) in
        let l = level_of_universe u in
        let graph = add_universe l ~lbound:mset uconv.graph in
        let graph =
          Universe.Map.fold
            (fun u' l' graph -> update_graph (l, u) (l', u') graph)
            uconv.levels graph
        in
        let graph =
          N.Map.fold
            (fun _ l' graph ->
               match l' with
               | LSProp -> graph
               | Level l' -> update_graph (l, u) (l', Universe.make l') graph)
            uconv.map graph
        in
        let uconv =
          { uconv with levels = Universe.Map.add u l uconv.levels; graph }
        in
        (uconv, l)))

type binder_kind =
  | NotImplicit
  | Maximal
  | NonMaximal
  | Typeclass  (** WRT Coq, Typeclass is like Maximal. *)

type expr =
  | Bound of int
  | Sort of U.t
  | Const of N.t * U.t list
  | App of expr * expr
  | Let of { name : N.t; ty : expr; v : expr; rest : expr }
      (** Let: undocumented in export_format.md *)
  | Lam of binder_kind * N.t * expr * expr
  | Pi of binder_kind * N.t * expr * expr
  | Proj of N.t * int * expr
    (** Proj: name of ind, field, term *)
  | Nat of Z.t
  | String of string

type def = { ty : expr; body : expr; univs : N.t list; height : int }

type ax = { ty : expr; univs : N.t list }

type ind = {
  params : (binder_kind * N.t * expr) list;
  ty : expr;
  ctors : (N.t * expr) list;
  univs : N.t list;
}

type entry = Def of def | Ax of ax | Ind of ind | Quot

(** Definitional height, used for unfolding heuristics.

   The definitional height is the longest sequence of constant
   unfoldings until we get a term without definitions (recursors don't
   count). *)
let height entries =
  let rec h = function
    | Const (c, _) ->
      (match N.Map.find c entries with
      | exception Not_found ->
        0 (* maybe a constructor or recursor, or just skipped *)
      | Def { height } -> height + 1
      | Quot | Ax _ | Ind _ -> 0)
    | Bound _ | Sort _ -> 0
    | Lam (_, _, a, b) | Pi (_, _, a, b) | App (a, b) -> max (h a) (h b)
    | Let { name = _; ty; v; rest } -> max (h ty) (max (h v) (h rest))
    | Proj (_, _, c) -> h c
    | Nat _ | String _ -> 0
  in
  h

type notation_kind = Prefix | Infix | Postfix

type notation = {
  kind : notation_kind;
  head : N.t;
  level : int;
  token : string;
}
[@@warning "-69"] (* not yet used *)

type instantiation = {
  ref : GlobRef.t;
  algs : Universe.t list;
      (** For each extra universe, produce the algebraic it corresponds to
          (the initial universes are replaced by the appropriate Var) *)
}

(*
Lean classifies inductives in the following way:
- inductive landing in always >Prop (even when instantiated to all Prop) -> never squashed
- inductive which has Prop instantiation:
  + no constructor -> never squashed
  + multi constructor -> always squashed
  + 1 constructor
    * no non param arguments
      -> not squashed, special reduction
      typically [eq]
    * all arguments appear in the output type
      -> not squashed, basic reduction
      no real world examples? eg [Inductive foo : nat -> Prop := bar : forall x, foo x.]
      2019 "type theory of lean" implies that there should be special reduction
      but testing says otherwise
    * some arguments don't appear in the output type, but are Prop or recursive
      -> not squashed, basic reduction
      typically [Acc], [and]
    * some non-Prop non-recursive arguments (for some instantiation) don't appear in the output type
      -> squashed
      typically [exists]

Additionally, the recursor is nondependent when the type is always Prop,
otherwise dependent (including for sometimes-Prop types)
(this implem detail isn't in the TTofLean paper)
Special reduction also seems restricted to always-Prop types.

NB: in practice (all stdlib and mathlib) the target universe is available without reduction
(ie syntactic arity) even though the system doesn't require it.
so we can just look at it directly (we don't want to implement a reduction system)

Difference with Coq:
- a non-Prop instantiation of possibly Prop types will never be squashed
- non squashed possibly-Prop types at a Prop instantiation are squashed
  (unless empty or uip branch)
- we need uip branch for the special reduction.
  TTofLean sounds like we need an encoding with primitive records
  but testing indicates otherwise (all args in output type case).
- we will always need unsafe flags for [Acc], and possibly for [and].

Instantiating the type with all-Prop may side-effect instantiate
some other globals with Prop that won't actually be used
(assuming the inductive is not used with all-Prop)
This probably doesn't matter much, also if we start using upfront
instantiations it won't matter at all.
*)

type squashy = {
  maybe_prop : bool;  (** used for optim, not fundamental *)
  always_prop : bool;
      (** controls whether the lean eliminator is dependent (and
              special reduction, but we just let Coq do its thing for that). *)
  lean_squashes : bool;
      (** Self descriptive. We handle necessity of unsafe flags per-instantiation. *)
}

let noprop = { maybe_prop = false; always_prop = false; lean_squashes = false }

let pp_squashy { maybe_prop; always_prop; lean_squashes } =
  let open Pp in
  (if maybe_prop then
   if always_prop then str "is always Prop" else str "may be Prop"
  else str "is never Prop")
  ++ spc ()
  ++
  if lean_squashes then str "and is squashed by Lean"
  else str "and is not squashed by Lean"

let coq_squashes graph (entry : Entries.mutual_inductive_entry) =
  let env = Global.env () in
  let env = Environ.set_universes graph env in
  let ind =
    match entry.mind_entry_inds with [ ind ] -> ind | _ -> assert false
  in
  let params = entry.mind_entry_params in
  let ty = ind.mind_entry_arity in
  let env_params = Environ.push_rel_context params env in
  let _, s = Reduction.dest_arity env_params ty in
  (* TODO merge with uip branch *)
  if not (Sorts.is_sprop s) then false
  else
    match ind.mind_entry_lc with
    | [] -> false
    | _ :: _ :: _ -> true
    | [ c ] -> (match Constr.kind c with Rel _ | App _ -> false | _ -> true)

let with_env_evm rels uconv f x =
  (* In non upfront mode,
     because we interleave defining new constants as we encounter them,
     pushing rels and handling local universes,
     we pass just the rel_context_val and merge it with the global env and the uconv here *)
  let env = Global.env () in
  let env = Environ.set_rel_context_val rels env in
  let env = Environ.set_universes uconv.graph env in
  let evd = Evd.from_env env in
  f env evd x

let to_annot rels n t uconv =
  (* In non upfront mode,
     because we interleave defining new constants as we encounter them,
     pushing rels and handling local universes,
     we pass just the rel_context_val and merge it with the global env and the uconv here *)
  let r = with_env_evm rels uconv Retyping.relevance_of_type (EConstr.of_constr t) in
  Context.make_annot (N.to_name n) r

(* bit n of [int_of_univs univs] is 1 iff [List.nth univs n] is SProp *)
let int_of_univs =
  let rec aux i acc = function
    | [] -> (i, acc)
    | u :: rest ->
      match univ_of_sort u with
      | None ->
      aux ((i * 2) + 1) acc rest
      | Some u ->
      aux (i * 2) (u :: acc) rest
  in
  fun l -> aux 0 [] (List.rev l)

let univ_of_name u =
  if lean_fancy_univs () then
    let u = DirPath.make [ N.to_id u; lean_id ] in
    Level.(make (UGlobal.make u "" 0))
  else UnivGen.fresh_level ()

let start_uconv univs i =
  let uconv =
    {
      graph = Global.universes ();
      map = N.Map.empty;
      levels = Universe.Map.empty;
    }
  in
  let uconv, set1 = level_of_sets uconv 1 in
  let rec aux uconv i = function
    | [] ->
      assert (i = 0);
      uconv
    | u :: univs ->
      let map, graph =
        if i mod 2 = 0 then
          let v = univ_of_name u in
          (N.Map.add u (Level v) uconv.map, add_universe v ~lbound:set1 uconv.graph)
        else (N.Map.add u LSProp uconv.map, uconv.graph)
      in
      aux { uconv with map; graph } (i / 2) univs
  in
  aux uconv i univs

let rec make_unames univs ounivs =
  match (univs, ounivs) with
  | _, [] ->
    List.map (fun u -> Name (Id.of_string_soft (Level.to_string u))) univs
  | _u :: univs, o :: ounivs -> N.to_name o :: make_unames univs ounivs
  | [], _ :: _ -> assert false

let univ_entry_gen { map; levels; graph } ounivs =
  let ounivs =
    CList.map_filter
      (fun u ->
        let v = N.Map.get u map in
        match v with
        | LSProp -> None
        | Level v -> Some (u, v))
      ounivs
  in
  let ounivs, univs = List.split ounivs in
  let univs, algs =
    if Universe.Map.is_empty levels then (univs, [])
    else
      let univs = List.rev univs in
      (* add the new levels to univs, add constraints between maxes
         (eg max(a,b) <= max(a,b,c)) *)
      let univs, algs =
        Universe.Map.fold
          (fun alg l (univs, algs) -> (l :: univs, alg :: algs))
          levels (univs, [])
      in
      let univs = List.rev univs in
      (univs, algs)
  in
  let uset = List.fold_left (fun kept l -> Level.Set.add l kept) Level.Set.empty univs in
  let kept = Level.Set.add Level.set uset in
  let kept = Int.Map.fold (fun _ -> Level.Set.add) !sets kept in
  let csts = UGraph.constraints_for ~kept graph in
  let csts =
    Constraints.filter (fun (a, _, b) -> Level.Set.mem a uset || Level.Set.mem b uset) csts
  in
  let unames = Array.of_list (make_unames univs ounivs) in
  let univs = Instance.of_array (Array.of_list univs) in
  let uctx = UContext.make unames (univs, csts) in
  let subst = make_instance_subst univs in
  let algs = List.rev_map (subst_univs_level_universe subst) algs in
  uctx, algs

let univ_entry a b =
  let uctx, algs = univ_entry_gen a b in
  ((UState.Polymorphic_entry uctx, UnivNames.empty_binders), algs)

(* TODO restrict univs (eg [has_add : Sort (u+1) -> Sort(u+1)] can
   drop the [u] and keep only the replacement for [u+1]??

   Preserve algebraics in codomain position? *)

let name_for_core n i =
  if i = 0 then N.to_id n
  else Id.of_string (N.to_coq_string n ^ "_inst" ^ string_of_int i)

(* NB collisions for constructors/recursors are still possible but
   should be rare *)
let name_for n i =
  let base = name_for_core n i in
  if not (Global.exists_objlabel (Label.of_id base)) then base
  else
    (* prevent resetting the number *)
    let base = if i = 0 then base else Id.of_string (Id.to_string base ^ "_") in
    Namegen.next_global_ident_away base Id.Set.empty

let get_predeclared_eq n i =
  if N.equal n (N.append N.anon "eq") then
    let ind_name = name_for_core n i in
    let reg = "lean." ^ Id.to_string ind_name in
    match Coqlib.lib_ref reg with
    | IndRef (ind, 0) -> Some (ind_name, ind)
    | _ ->
      CErrors.user_err
        Pp.(
          str "Bad registration for "
          ++ str reg
          ++ str " expected an inductive.")
    | exception _ -> None
  else None

(** For each name, the instantiation with all non-sprop univs should
   always be declared, but the instantiations with SProp may be lazily
   declared. We expect small instance lengths (experimentally at most
   4 in the stdlib) so we represent instantiations as bit fields, bit
   n is 1 iff universe n is instantiated by SProp. *)
let declared : instantiation Int.Map.t N.Map.t ref =
  Summary.ref ~name:"lean-declared-instances" N.Map.empty

let entries : entry N.Map.t ref = Summary.ref ~name:"lean-entries" N.Map.empty

let squash_info : squashy N.Map.t ref =
  Summary.ref ~name:"lean-squash-info" N.Map.empty

let add_declared n i inst =
  declared :=
    N.Map.update n
      (function
        | None -> Some (Int.Map.singleton i inst)
        | Some m -> Some (Int.Map.add i inst m))
      !declared

let to_univ_level' u uconv =
  match to_universe uconv.map u with
  | SProp -> uconv, LSProp
  | Type u | QSort (_, u) ->
    let uconv, u = to_univ_level u uconv in
    uconv, Level u
  | Set -> uconv, Level Level.set
  | Prop -> assert false

let empty_env = Environ.empty_rel_context_val

let rec to_constr =
  let open Constr in
  let ( >>= ) x f uconv =
    let uconv, x = x uconv in
    f x uconv
  in
  let get_uconv uconv = uconv, uconv in
  let ret x uconv = (uconv, x) in
  let to_annot env n t u = u, to_annot env n t u in
  let push_rel = Environ.push_rel_context_val in
  fun env -> function
  | Bound i -> ret (mkRel (i + 1))
  | Sort univ ->
    to_univ_level' univ >>= fun u ->
    ret (mkSort (sort_of_level u))
  | Const (n, univs) -> instantiate n univs
  | App (a, b) ->
    to_constr env a >>= fun a ->
    to_constr env b >>= fun b -> ret (mkApp (a, [| b |]))
  | Let { name; ty; v; rest } ->
    to_constr env ty >>= fun ty ->
    to_annot env name ty >>= fun name ->
    to_constr env v >>= fun v ->
    to_constr (push_rel (LocalDef (name, v, ty)) env) rest >>= fun rest ->
    ret (mkLetIn (name, v, ty, rest))
  | Lam (_bk, n, a, b) ->
    to_constr env a >>= fun a ->
    to_annot env n a >>= fun n ->
    to_constr (push_rel (LocalAssum (n, a)) env) b >>= fun b ->
    ret (mkLambda (n, a, b))
  | Pi (_bk, n, a, b) ->
    to_constr env a >>= fun a ->
    to_annot env n a >>= fun n ->
    to_constr (push_rel (LocalAssum (n, a)) env) b >>= fun b ->
    ret (mkProd (n, a, b))
  | Proj (_ind, field, c) ->
    to_constr env c >>= fun c ->
    get_uconv >>= fun uconv ->
    (* we retype to get the ind, because otherwise we need the lean
       univs for instantiation
       This means we ignore the ind in the Proj data. *)
    let ind = with_env_evm env uconv (fun env evd () ->
        let c = EConstr.of_constr c in
        let tc = Retyping.get_type_of env evd c in
        let tc = fst (Termops.decompose_app_vect evd (Reductionops.whd_all env evd tc)) in
        match Constr.kind (EConstr.Unsafe.to_constr tc) with
        | Ind (ind,_) -> ind
        | _ -> assert false)
        ()
    in
    let p = Environ.get_projection (Global.env()) ind ~proj_arg:field in
    (* unfolded?? *)
    ret (mkProj (Projection.make p false, c))
  | Nat _ -> CErrors.user_err Pp.(str "TODO native nat")
  | String _ -> CErrors.user_err Pp.(str "TODO native string")

and instantiate n univs uconv =
  assert (List.length univs < Sys.int_size);
  (* TODO what happens when is_large_elim and the motive is instantiated with Prop? *)
  let univs = List.map (to_universe uconv.map) univs in
  let i, univs = int_of_univs univs in
  let inst = ensure_exists n i in
  let subst l =
    match Level.var_index l with
    | None -> Universe.make l
    | Some n -> List.nth univs n
  in
  let extra =
    List.map
      (fun alg -> simplify_universe (UnivSubst.subst_univs_universe subst alg))
      inst.algs
  in
  let univs = List.concat [ univs; extra ] in
  let uconv, univs =
    CList.fold_left_map (fun uconv u -> to_univ_level u uconv) uconv univs
  in
  let u = Instance.of_array (Array.of_list univs) in
  (uconv, Constr.mkRef (inst.ref, u))

and ensure_exists n i =
  try !declared |> N.Map.find n |> Int.Map.find i
  with Not_found ->
    (* TODO can we end up asking for a ctor or eliminator before
       asking for the inductive type? *)
    (* if i = 0 then CErrors.user_err Pp.(N.pp n ++ str " was not instantiated!"); *)
    (* assert (not (upfront_instances ())); *)
    (match N.Map.get n !entries with
    | Def def -> declare_def n def i
    | Ax ax -> declare_ax n ax i
    | Ind ind -> declare_ind n ind i
    | Quot -> CErrors.user_err Pp.(str "quot must be predeclared")
    | exception _ -> CErrors.user_err Pp.(str "missing "++N.pp n ))

and declare_def n { ty; body; univs; height } i =
  let uconv = start_uconv univs i in
  let uconv, ty = to_constr empty_env ty uconv in
  let uconv, body = to_constr empty_env body uconv in
  let univs, algs = univ_entry uconv univs in
  let ref = try quickdef ~name:(name_for n i) ~types:(Some ty) ~univs body
    with e ->
      let e = Exninfo.capture e in
      Feedback.msg_info Pp.(str "Failed with" ++ fnl() ++ Printer.pr_constr_env (Global.env()) (Evd.from_env (Global.env())) body ++ fnl() ++ str ": " ++ Printer.pr_constr_env (Global.env()) (Evd.from_env (Global.env())) ty);
      Exninfo.iraise e
  in
  let () =
    let c = match ref with ConstRef c -> c | _ -> assert false in
    Global.set_strategy (ConstKey c) (Level (-height))
  in
  let inst = { ref; algs } in
  let () = add_declared n i inst in
  inst

and declare_ax n { ty; univs } i =
  let uconv = start_uconv univs i in
  let uconv, ty = to_constr empty_env ty uconv in
  let univs, algs = univ_entry uconv univs in
  let entry = Declare.(ParameterEntry (parameter_entry ~univs ty)) in
  let c =
    Declare.declare_constant ~name:(name_for n i)
      ~kind:Decls.(IsAssumption Definitional)
      entry
  in
  let inst = { ref = GlobRef.ConstRef c; algs } in
  let () = add_declared n i inst in
  inst

and to_params uconv params =
  let acc, params =
    CList.fold_left_map
      (fun (env,uconv) (_bk, p, ty) ->
         let uconv, ty = to_constr env ty uconv in
         let d = RelDecl.LocalAssum (to_annot env p ty uconv, ty) in
         let env = Environ.push_rel_context_val d env in
         ((env,uconv), d))
      (empty_env,uconv) params
  in
  (acc, List.rev params)

and declare_ind n { params; ty; ctors; univs } i =
  let mind, algs, ind_name, cnames, univs, squashy =
    match get_predeclared_eq n i with
    | Some (ind_name, mind) ->
      (* Hack to let the user predeclare eq and quot before running Lean Import
         TODO make a more general Register-like API? *)
      Feedback.msg_info Pp.(Id.print ind_name ++ str " is predeclared");
      let cname = N.append n "refl" in
      let squashy =
        { maybe_prop = true; always_prop = true; lean_squashes = false }
      in
      let univs =
        match i with
        | 0 ->
          UContext.make [| Name (Id.of_string "u") |]
            ( Instance.of_array [| univ_of_name (N.append N.anon "u") |],
              Constraints.empty )
        | 1 -> UContext.empty
        | _ -> assert false
      in
      (mind, [], ind_name, [ cname ], univs, squashy)
    | None ->
      let uconv = start_uconv univs i in
      let (env_params,uconv), params = to_params uconv params in
      let uconv, ty = to_constr env_params ty uconv in
      let _, sort =
        let env_params =
          Environ.set_rel_context_val env_params
            (Environ.set_universes uconv.graph (Global.env ()))
        in
        Reduction.dest_arity env_params ty
      in
      let env_ind = Environ.push_rel_context_val
          (LocalAssum
             ((Context.make_annot (N.to_name n)
                 (Sorts.relevance_of_sort sort)),
              Term.it_mkProd_or_LetIn ty params))
          empty_env
      in
      let env_ind_params =
        Context.Rel.fold_outside Environ.push_rel_context_val params
          ~init:env_ind
      in
      let uconv, ctors =
        CList.fold_left_map
          (fun uconv (n, ty) ->
            let uconv, ty = to_constr env_ind_params ty uconv in
            (uconv, (n, ty)))
          uconv ctors
      in
      let cnames, ctys = List.split ctors in
      let graph = uconv.graph in
      let univs, algs = univ_entry_gen uconv univs in
      let ind_name = name_for n i in
      let entry =
        {
          Entries.mind_entry_params = params;
          mind_entry_record = None;
          mind_entry_finite = Finite;
          mind_entry_inds =
            [
              {
                mind_entry_typename = ind_name;
                mind_entry_arity = ty;
                mind_entry_consnames = List.map (fun n -> name_for n i) cnames;
                mind_entry_lc = ctys;
              };
            ];
          mind_entry_private = None;
          mind_entry_universes = Polymorphic_ind_entry univs;
          mind_entry_variance = None;
        }
      in
      let squashy = N.Map.get n !squash_info in
      let coq_squashes =
        if squashy.maybe_prop then coq_squashes graph entry else false
      in
      let mind =
        let act () =
          DeclareInd.declare_mutual_inductive_with_eliminations entry
            (* the ubinders API is kind of shit here *)
            (UState.Polymorphic_entry UContext.empty,UnivNames.empty_binders)
            []
        in
        if squashy.lean_squashes || not coq_squashes then act ()
        else with_unsafe_univs act ()
      in
      assert (
        squashy.lean_squashes
        || (Global.lookup_mind mind).mind_packets.(0).mind_kelim == InType);
      (mind, algs, ind_name, cnames, univs, squashy)
  in

  (* add ind and ctors to [declared] *)
  let inst = { ref = GlobRef.IndRef (mind, 0); algs } in
  let () = add_declared n i inst in
  let () =
    CList.iteri
      (fun cnum cname ->
        add_declared cname i
          { ref = GlobRef.ConstructRef ((mind, 0), cnum + 1); algs })
      cnames
  in

  (* elim *)
  let make_scheme fam =
    let u =
      if fam = Sorts.InSProp then LSProp
      else
      let u = if lean_fancy_univs () then
          let u = DirPath.make [ Id.of_string "motive"; lean_id ] in
          Level.(make (UGlobal.make u "" 0))
        else UnivGen.fresh_level ()
      in
      Level u
    in
    let env = Environ.push_context ~strict:true univs (Global.env ()) in
    let env =
      match u with
      | LSProp -> env
      | Level u -> Environ.push_context_set ~strict:false (ContextSet.singleton u) env
    in
    let inst, uentry =
      let inst = UContext.instance univs in
      let csts = UContext.constraints univs in
      let names = UContext.names univs in
      let uentry = match u with
        | LSProp -> UState.Polymorphic_entry univs
        | Level u ->
          UState.Polymorphic_entry
            (UContext.make (Array.append [| Name (Id.of_string "motive") |] names)
               ( Instance.of_array
                   (Array.append [| u |] (Instance.to_array inst)),
                 csts ) )
      in
      ( inst, uentry )
    in
    (* lean 4 change: always dep? was not squashy.always_prop*)
    (lean_scheme env ~dep:true mind inst u, uentry)
  in
  let nrec = N.append n "rec" in
  let elims =
    if squashy.lean_squashes then [ ("_indl", Sorts.InSProp) ]
    else [ ("_recl", InType); ("_indl", InSProp) ]
  in
  let () =
    List.iter
      (fun (suffix, sort) ->
        let id = Id.of_string (Id.to_string ind_name ^ suffix) in
        let body, uentry = make_scheme sort in
        let elim =
          quickdef ~name:id ~types:None ~univs:(uentry, UnivNames.empty_binders) body
          (* TODO implicits? *)
        in
        (* TODO AFAICT Lean reduces recursors eagerly, but ofc only when applied to a ctor
           Can we simulate that with strategy better than by leaving them at the default strat? *)
        let liftu l =
          match Level.var_index l with
          | None -> Universe.make l (* Set *)
          | Some i -> Universe.make (Level.var (i + 1))
        in
        let algs =
          if sort = InSProp then algs
          else List.map (UnivSubst.subst_univs_universe liftu) algs
        in
        let elim = { ref = elim; algs } in
        let j =
          if squashy.lean_squashes then i
          else if sort == InType then 2 * i
          else (2 * i) + 1
        in
        add_declared nrec j elim)
      elims
  in
  inst

(** Generate and add the squashy info *)
let squashify n { params; ty; ctors; univs } =
  let uconvP =
    (* NB: if univs = [] this is just instantiation 0 *)
    start_uconv univs ((1 lsl List.length univs) - 1)
  in
  let (env_paramsP,uconvP), paramsP = to_params uconvP params in
  let uconvP, tyP = to_constr env_paramsP ty uconvP in
  let envP =
    Environ.push_rel_context paramsP
      (Environ.set_universes uconvP.graph (Global.env ()))
  in
  let _, sortP = Reduction.dest_arity envP tyP in
  if not (Sorts.is_sprop sortP) then noprop
  else
    let uconvT = start_uconv univs 0 in
    let (env_paramsT,uconvT), paramsT = to_params uconvT params in
    let uconvT, tyT = to_constr env_paramsT ty uconvT in
    let envT =
      Environ.set_rel_context_val env_paramsT
        (Environ.set_universes uconvT.graph (Global.env ()))
    in
    let _, sortT = Reduction.dest_arity envT tyT in
    let always_prop = Sorts.is_sprop sortT in
    match ctors with
    | [] -> { maybe_prop = true; always_prop; lean_squashes = false }
    | _ :: _ :: _ -> { maybe_prop = true; always_prop; lean_squashes = true }
    | [ (_, ctor) ] ->
      let envT =
        Context.Rel.fold_outside Environ.push_rel_context_val paramsT
          ~init:(Environ.push_rel_context_val
                   (LocalAssum
                      (Context.make_annot (N.to_name n)
                         (Sorts.relevance_of_sort sortT),
                       Term.it_mkProd_or_LetIn tyT paramsT))
                   empty_env)
      in
      let uconvT, ctorT = to_constr envT ctor uconvT in
      let envT =
        Environ.set_rel_context_val envT
          (Environ.set_universes uconvT.graph (Global.env ()))
      in
      let args, out = Reduction.hnf_decompose_prod envT ctorT in
      let forced =
        (* NB dest_prod returns [out] in whnf *)
        let _, outargs = Constr.decompose_appvect out in
        Array.fold_left
          (fun forced arg ->
            match Constr.kind arg with
            | Rel i -> Int.Set.add i forced
            | _ -> forced)
          Int.Set.empty outargs
      in
      let sigma = Evd.from_env envT in
      let npars = List.length params in
      let nargs = List.length args in
      let lean_squashes, _, _ =
        Context.Rel.fold_outside
          (fun d (squashed, i, envT) ->
            let squashed =
              if squashed then true
              else if Int.Set.mem (nargs - i) forced then false
              else
                let t = RelDecl.get_type d in
                if not (Vars.noccurn (npars + i + 1) t) then
                  (* recursive argument *)
                  false
                else
                  not
                    (EConstr.ESorts.is_sprop sigma
                       (Retyping.get_sort_of envT sigma (EConstr.of_constr t)))
            in

            (squashed, i + 1, Environ.push_rel d envT))
          args ~init:(false, 0, envT)
      in
      (* TODO translate to use non recursively uniform params (fix extraction)*)
      { maybe_prop = true; always_prop; lean_squashes }

let squashify n ind =
  let s = squashify n ind in
  squash_info := N.Map.add n s !squash_info

let quot_name = N.append N.anon "quot"

(* pairs of (name * number of univs) *)
let quots = [ ("", 1); ("mk", 1); ("lift", 2); ("ind", 1) ]

let declare_quot () =
  let () =
    List.iter
      (fun (n, nunivs) ->
        let rec loop i =
          if i = 1 lsl nunivs then ()
          else
            let lean =
              if CString.is_empty n then quot_name else N.append quot_name n
            in
            let reg =
              "lean." ^ N.to_lean_string lean
              ^ if i = 0 then "" else "_inst" ^ string_of_int i
            in
            let ref = Coqlib.lib_ref reg in
            let () = add_declared lean i { ref; algs = [] } in
            loop (i + 1)
        in
        loop 0)
      quots
  in
  Feedback.msg_info Pp.(str "quot registered")

exception MissingQuot

let declare_quot () =
  if Coqlib.has_ref "lean.quot" then declare_quot () else raise MissingQuot

let do_bk = function
  | "#BD" -> NotImplicit
  | "#BI" -> Maximal
  | "#BS" -> NonMaximal
  | "#BC" -> Typeclass
  | bk ->
    CErrors.user_err
      Pp.(str "unknown binder kind " ++ str bk ++ str "." ++ fnl ())

let do_notation_kind = function
  | "#PREFIX" -> Prefix
  | "#INFIX" -> Infix
  | "#POSTFIX" -> Postfix
  | k -> assert false

type parsing_state = {
  names : N.t RRange.t;
  exprs : expr RRange.t;
  univs : U.t RRange.t;
  skips : int;
  notations : notation list;
}

let empty_state =
  {
    names = RRange.singleton N.anon;
    exprs = RRange.empty;
    univs = RRange.singleton U.Prop;
    skips = 0;
    notations = [];
  }

let get_name state n =
  let n = int_of_string n in
  RRange.get state.names n

let get_expr state e =
  let e = int_of_string e in
  RRange.get state.exprs e

let rec do_ctors state nctors acc l =
  if nctors = 0 then (List.rev acc, l)
  else
    match l with
    | name :: ty :: rest ->
      let name = get_name state name
      and ty = get_expr state ty in
      do_ctors state (nctors - 1) ((name, ty) :: acc) rest
    | _ -> CErrors.user_err Pp.(str "Not enough constructors")

(** Replace [n] (meant to be an the inductive type appearing in the
    constructor type) by (Bound k). *)
let rec replace_ind ind k = function
  | Const (n', _) when N.equal ind n' -> Bound k
  | (Const _ | Bound _ | Sort _) as e -> e
  | App (a, b) -> App (replace_ind ind k a, replace_ind ind k b)
  | Let { name; ty; v; rest } ->
    Let
      {
        name;
        ty = replace_ind ind k ty;
        v = replace_ind ind k v;
        rest = replace_ind ind (k + 1) rest;
      }
  | Lam (bk, name, a, b) ->
    Lam (bk, name, replace_ind ind k a, replace_ind ind (k + 1) b)
  | Pi (bk, name, a, b) ->
    Pi (bk, name, replace_ind ind k a, replace_ind ind (k + 1) b)
  | Proj (n, field, c) ->
    Proj (n, field, replace_ind ind k c)
  | Nat _ | String _ as x -> x

let rec pop_params npar ty =
  if npar = 0 then ([], ty)
  else
    match ty with
    | Pi (bk, name, a, b) ->
      let pars, ty = pop_params (npar - 1) b in
      ((bk, name, a) :: pars, ty)
    | _ -> assert false

let fix_ctor ind nparams ty =
  let _, ty = pop_params nparams ty in
  replace_ind ind nparams ty

let as_univ state s = RRange.get state.univs (int_of_string s)

let just_parse =
  Goptions.declare_bool_option_and_ref ~depr:false
    ~stage:Interp
    ~key:[ "Lean"; "Just"; "Parsing" ]
    ~value:false

(* with this off: best line 23000 in stdlib
   stack overflow

   update: got fixed by e9e637de26 (distinguish names foo.bar and foo_bar)
*)
let upfront_instances =
  Goptions.declare_bool_option_and_ref ~depr:false
    ~stage:Interp
    ~key:[ "Lean"; "Upfront"; "Instantiation" ]
    ~value:false

let lazy_instances =
  Goptions.declare_bool_option_and_ref ~depr:false
    ~stage:Interp
    ~key:[ "Lean"; "Lazy"; "Instantiation" ]
    ~value:false

let declare_instances act univs =
  let stop = if upfront_instances () then 1 lsl List.length univs else 1 in
  let rec loop i =
    if i = stop then ()
    else
      let () = act i in
      loop (i + 1)
  in
  if not (lazy_instances ()) then loop 0

let declare_def name def =
  declare_instances (fun i -> ignore (declare_def name def i)) def.univs

let declare_ax name ax =
  declare_instances (fun i -> ignore (declare_ax name ax i)) ax.univs

let declare_ind name ind =
  let () = squashify name ind in
  declare_instances (fun i -> ignore (declare_ind name ind i)) ind.univs

let add_entry n entry =
  let () =
    match entry with
    | Quot -> declare_quot ()
    | Def def -> declare_def n def
    | Ax ax -> declare_ax n ax
    | Ind ind -> declare_ind n ind
  in
  entries := N.Map.add n entry !entries

let lcnt = ref 0

let line_msg name =
  Feedback.msg_info Pp.(str "line " ++ int !lcnt ++ str ": " ++ N.pp name)

let parse_hexa c =
  if 'A' <= c && c <= 'F' then int_of_char c - int_of_char 'A'
  else begin
    assert ('0' <= c && c <= '9');
    int_of_char c - int_of_char '0'
  end

let parse_char s =
  assert (String.length s = 2);
  Char.chr (parse_hexa s.[0] * 16 + parse_hexa s.[1])

let do_line state l =
  (* Lean printing strangeness: sometimes we get double spaces (typically with INFIX) *)
  match
    List.filter (fun s -> s <> "") (String.split_on_char ' ' (String.trim l))
  with
  | [] -> (state, None) (* empty line *)
  | "#DEF" :: name :: ty :: body :: univs ->
    let name = get_name state name in
    line_msg name;
    let ty = get_expr state ty
    and body = get_expr state body
    and univs = List.map (get_name state) univs in
    let height = height !entries body in
    let def = { ty; body; univs; height } in
    (state, Some (name, Def def))
  | "#AX" :: name :: ty :: univs ->
    let name = get_name state name in
    line_msg name;
    let ty = get_expr state ty
    and univs = List.map (get_name state) univs in
    let ax = { ty; univs } in
    (state, Some (name, Ax ax))
  | "#IND" :: nparams :: name :: ty :: nctors :: rest ->
    let name = get_name state name in
    line_msg name;
    let nparams = int_of_string nparams
    and ty = get_expr state ty
    and nctors = int_of_string nctors in
    let params, ty = pop_params nparams ty in
    let ctors, univs = do_ctors state nctors [] rest in
    let ctors =
      List.map (fun (nctor, ty) -> (nctor, fix_ctor name nparams ty)) ctors
    in
    let univs = List.map (get_name state) univs in
    let ind = { params; ty; ctors; univs } in
    (state, Some (name, Ind ind))
  | [ "#QUOT" ] ->
    line_msg quot_name;
    (state, Some (quot_name, Quot))
  | (("#PREFIX" | "#INFIX" | "#POSTFIX") as kind) :: rest ->
    (match rest with
    | [ n; level; token ] ->
      let kind = do_notation_kind kind
      and n = get_name state n
      and level = int_of_string level in
      ( {
          state with
          notations = { kind; head = n; level; token } :: state.notations;
        },
        None )
    | _ ->
      CErrors.user_err
        Pp.(
          str "bad notation: " ++ prlist_with_sep (fun () -> str "; ") str rest))
  | next :: rest ->
    let next = int_of_string next in
    let state =
      match rest with
      | [ "#NS"; base; cons ] ->
        assert (next = RRange.length state.names);
        let base = get_name state base in
        let cons = N.append base cons in
        { state with names = RRange.append state.names cons }
      | [ "#NI"; base; cons ] ->
        assert (next = RRange.length state.names);
        (* NI: private name. cons is an int, base is expected to be _private :: stuff
           (true in lean stdlib, dunno elsewhere) *)
        let base = get_name state base in
        let n = N.raw_append base cons in
        { state with names = RRange.append state.names n }
      | [ "#US"; base ] ->
        assert (next = RRange.length state.univs);
        let base = as_univ state base in
        { state with univs = RRange.append state.univs (Succ base) }
      | [ "#UM"; a; b ] ->
        assert (next = RRange.length state.univs);
        let a = as_univ state a
        and b = as_univ state b in
        { state with univs = RRange.append state.univs (Max (a, b)) }
      | [ "#UIM"; a; b ] ->
        assert (next = RRange.length state.univs);
        let a = as_univ state a
        and b = as_univ state b in
        { state with univs = RRange.append state.univs (IMax (a, b)) }
      | [ "#UP"; n ] ->
        assert (next = RRange.length state.univs);
        let n = get_name state n in
        { state with univs = RRange.append state.univs (UNamed n) }
      | [ "#EV"; n ] ->
        assert (next = RRange.length state.exprs);
        let n = int_of_string n in
        { state with exprs = RRange.append state.exprs (Bound n) }
      | [ "#ES"; u ] ->
        assert (next = RRange.length state.exprs);
        let u = as_univ state u in
        { state with exprs = RRange.append state.exprs (Sort u) }
      | "#EC" :: n :: univs ->
        let n = get_name state n in
        assert (next = RRange.length state.exprs);
        let univs = List.map (as_univ state) univs in
        { state with exprs = RRange.append state.exprs (Const (n, univs)) }
      | [ "#EA"; a; b ] ->
        assert (next = RRange.length state.exprs);
        let a = get_expr state a
        and b = get_expr state b in
        { state with exprs = RRange.append state.exprs (App (a, b)) }
      | [ "#EZ"; n; ty; v; rest ] ->
        assert (next = RRange.length state.exprs);
        let n = get_name state n
        and ty = get_expr state ty
        and v = get_expr state v
        and rest = get_expr state rest in
        {
          state with
          exprs = RRange.append state.exprs (Let { name = n; ty; v; rest });
        }
      | [ "#EL"; bk; n; ty; body ] ->
        assert (next = RRange.length state.exprs);
        let bk = do_bk bk
        and n = get_name state n
        and ty = get_expr state ty
        and body = get_expr state body in
        { state with exprs = RRange.append state.exprs (Lam (bk, n, ty, body)) }
      | [ "#EP"; bk; n; ty; body ] ->
        assert (next = RRange.length state.exprs);
        let bk = do_bk bk
        and n = get_name state n
        and ty = get_expr state ty
        and body = get_expr state body in
        { state with exprs = RRange.append state.exprs (Pi (bk, n, ty, body)) }
      | [ "#EJ"; ind; field; term ] ->
        let ind = get_name state ind
        and field = int_of_string field
        and term = get_expr state term in
        { state with exprs = RRange.append state.exprs (Proj (ind, field, term)) }
      | [ "#ELN"; n ] ->
        let n = Z.of_string n in
        { state with exprs = RRange.append state.exprs (Nat n) }
      | "#ELS" :: bytes ->
        let s = Seq.map parse_char (List.to_seq bytes) in
        let s = String.of_seq s in
        { state with exprs = RRange.append state.exprs (String s) }
      | _ ->
        CErrors.user_err
          Pp.(str "cannot understand " ++ str l ++ str "." ++ fnl ())
    in
    (state, None)

let rec is_arity = function
  | Sort _ -> true
  | Pi (_, _, _, b) -> is_arity b
  | _ -> false

let print_squashes =
  Goptions.declare_bool_option_and_ref ~depr:false
    ~stage:Interp
    ~key:[ "Lean"; "Print"; "Squash"; "Info" ]
    ~value:false

let skip_missing_quot =
  Goptions.declare_bool_option_and_ref ~depr:false
    ~stage:Interp
    ~key:[ "Lean"; "Skip"; "Missing"; "Quotient" ]
    ~value:true

type error_mode =
  | Skip
  | Stop
  | Fail

let error_mode =
  let print = function
    | Skip -> "Skip"
    | Stop -> "Stop"
    | Fail -> "Fail"
  in
  let interp = function
    | "Skip" -> Skip
    | "Stop" -> Stop
    | "Fail" -> Fail
    | s -> CErrors.user_err Pp.(str "Unknown error mode " ++ qstring s ++ str ".")
  in
  Goptions.declare_interpreted_string_option_and_ref ~depr:false
    ~stage:Interp
    ~key:["Lean";"Error";"Mode"]
    ~value:Fail
    interp print

let error_mode = function
  | MissingQuot when skip_missing_quot () -> Skip
  | _ -> error_mode ()

let finish state =
  let max_univs, cnt =
    N.Map.fold
      (fun _ entry (m, cnt) ->
        match entry with
        | Ax { univs } | Def { univs } | Ind { univs } ->
          let l = List.length univs in
          (max m l, cnt + (1 lsl l))
        | Quot -> (max m 1, cnt + 2))
      !entries (0, 0)
  in
  let nonarities =
    N.Map.fold
      (fun _ entry cnt ->
        match entry with
        | Ax _ | Def _ | Quot -> cnt
        | Ind ind -> if is_arity ind.ty then cnt else cnt + 1)
      !entries 0
  in
  let squashes =
    if not (print_squashes ()) then Pp.mt ()
    else
      N.Map.fold
        (fun n s pp -> Pp.(pp ++ fnl () ++ N.pp n ++ spc () ++ pp_squashy s))
        !squash_info
        Pp.(mt ())
  in
  Feedback.msg_info
    Pp.(
      fnl () ++ fnl () ++ str "Done!" ++ fnl () ++ str "- "
      ++ int (N.Map.cardinal !entries)
      ++ str " entries (" ++ int cnt ++ str " possible instances)"
      ++ (if N.Map.exists (fun _ x -> Quot == x) !entries then
          str " (including quot)."
         else str ".")
      ++ fnl () ++ str "- "
      ++ int (RRange.length state.univs)
      ++ str " universe expressions"
      ++ fnl () ++ str "- "
      ++ int (RRange.length state.names)
      ++ str " names" ++ fnl () ++ str "- "
      ++ int (RRange.length state.exprs)
      ++ str " expression nodes" ++ fnl ()
      ++ (if state.skips > 0 then str "Skipped " ++ int state.skips ++ fnl ()
         else mt ())
      ++ str "Max universe instance length "
      ++ int max_univs ++ str "." ++ fnl () ++ int nonarities
      ++ str " inductives have non syntactically arity types."
      ++ squashes)

let prtime t0 t1 =
  let diff = System.time_difference t0 t1 in
  if diff > 1.0 then
    Feedback.msg_info
      Pp.(
        str "line " ++ int !lcnt ++ str " took "
        ++ System.fmt_time_difference t0 t1)

let timeout = ref None

let () =
  Goptions.declare_int_option
    {
      optdepr = false;
      optstage = Interp;
      optkey = [ "Lean"; "Line"; "Timeout" ];
      optread = (fun () -> !timeout);
      optwrite = (fun x -> timeout := x);
    }

exception TimedOut

let do_line state l =
  match !timeout with
  | None -> do_line state l
  | Some t ->
    (match Control.timeout (float_of_int t) (fun () -> do_line state l) () with
    | Some v -> v
    | None -> raise TimedOut)

let do_line state l =
  let t0 = System.get_time () in
  match do_line state l with
  | state ->
    let t1 = System.get_time () in
    prtime t0 t1;
    state
  | exception e ->
    let e = Exninfo.capture e in
    (if fst e <> TimedOut then
     let t1 = System.get_time () in
     prtime t0 t1);
    Exninfo.iraise e



let before_from = function None -> false | Some from -> !lcnt < from

let freeze () = (Lib.freeze (), Summary.freeze_summaries ~marshallable:false)

let unfreeze (lib, sum) =
  Lib.unfreeze lib;
  Summary.unfreeze_summaries sum

let rec do_input state ~from ~until ch =
  if until = Some !lcnt then begin
    close_in ch;
    finish state;
    state
  end
  else
    match input_line ch with
    | exception End_of_file ->
      close_in ch;
      finish state;
      if not (until = None) then CErrors.user_err Pp.(str "unexpected EOF!");
      state
    | _ when before_from from ->
      incr lcnt;
      do_input state ~from ~until ch
    | l ->
      let state, oentry = do_line state l in
      (match (just_parse (), oentry) with
      | true, _ | false, None ->
        incr lcnt;
        do_input state ~from ~until ch
      | false, Some (n, entry) ->
        (* freeze is actually pretty costly, so make sure we don't run it for non sideffect lines. *)
        let st = freeze () in
        (match add_entry n entry with
        | () ->
          incr lcnt;
          do_input state ~from ~until ch
        | exception e ->
          let e = Exninfo.capture e in
          let epp =
              Pp.(str "Error at line " ++ int !lcnt
                  ++ str " (for " ++ N.pp n ++ str ")"
                  ++ str (": " ^ l)
                  ++ fnl () ++ CErrors.iprint e)
          in
          unfreeze st;
          (* without this unfreeze, the global state.declared and the
             global env are out of sync *)
          match error_mode (fst e) with
          | Skip ->
            Feedback.msg_info
              Pp.(
                str "Skipping: " ++ epp);
            incr lcnt;
            do_input { state with skips = state.skips + 1 } ~from ~until ch
          | Stop ->
            close_in ch;
            finish state;
            Feedback.msg_info epp;
            state
          | Fail ->
            close_in ch;
            finish state;
            CErrors.user_err epp))

let pstate = Summary.ref ~name:"lean-parse-state" empty_state

let lean_obj =
  let cache (pstatev,setsv,declaredv,entriesv,squash_infov) =
    pstate := pstatev;
    sets := setsv;
    declared := declaredv;
    entries := entriesv;
    squash_info := squash_infov;
    ()
  in
  let open Libobject in
  declare_object {
    (default_object "LEAN-IMPORT-STATE") with
    cache_function = cache;
    load_function = (fun _ v -> cache v);
    classify_function = (fun _ -> Keep);
  }

let import ~from ~until f =
  lcnt := 1;
  (* silence the definition messages from Coq *)
  let pstatev = (Flags.silently (fun () -> do_input !pstate ~from ~until (open_in f)) ()) in
  Lib.add_leaf (lean_obj (pstatev, !sets, !declared, !entries, !squash_info))
