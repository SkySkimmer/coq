(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

module LargeArray :
sig
  type 'a t
  val make : int -> 'a -> 'a t
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
end =
struct

  let max_length = Sys.max_array_length

  type 'a t = 'a array array * 'a array
  (** Invariants:
      - All subarrays of the left array have length [max_length].
      - The right array has length < [max_length].
  *)

  let make n x =
    let k = n / max_length in
    let r = n mod max_length in
    let vl = Array.init k (fun _ -> Array.make max_length x) in
    let vr = Array.make r x in
    (vl, vr)

  let get (vl, vr) n =
    let k = n / max_length in
    let r = n mod max_length in
    let len = Array.length vl in
    if k < len then vl.(k).(r)
    else if k == len then vr.(r)
    else invalid_arg "index out of bounds"

  let set (vl, vr) n x =
    let k = n / max_length in
    let r = n mod max_length in
    let len = Array.length vl in
    if k < len then vl.(k).(r) <- x
    else if k == len then vr.(r) <- x
    else invalid_arg "index out of bounds"

end

let prefix_small_block =         0x80
let prefix_small_int =           0x40
let prefix_small_string =        0x20

[@@@ocaml.warning "-32"]
let code_int8 =                  0x00
let code_int16 =                 0x01
let code_int32 =                 0x02
let code_int64 =                 0x03
let code_shared8 =               0x04
let code_shared16 =              0x05
let code_shared32 =              0x06
let code_double_array32_little = 0x07
let code_block32 =               0x08
let code_string8 =               0x09
let code_string32 =              0x0A
let code_double_big =            0x0B
let code_double_little =         0x0C
let code_double_array8_big =     0x0D
let code_double_array8_little =  0x0E
let code_double_array32_big =    0x0F
let code_codepointer =           0x10
let code_infixpointer =          0x11
let code_custom =                0x12
let code_block64 =               0x13
let code_shared64 =              0x14
let code_string64 =              0x15
let code_double_array64_big =    0x16
let code_double_array64_little = 0x17
let code_custom_len =            0x18
let code_custom_fixed =          0x19

[@@@ocaml.warning "-37"]
type code_descr =
| CODE_INT8
| CODE_INT16
| CODE_INT32
| CODE_INT64
| CODE_SHARED8
| CODE_SHARED16
| CODE_SHARED32
| CODE_DOUBLE_ARRAY32_LITTLE
| CODE_BLOCK32
| CODE_STRING8
| CODE_STRING32
| CODE_DOUBLE_BIG
| CODE_DOUBLE_LITTLE
| CODE_DOUBLE_ARRAY8_BIG
| CODE_DOUBLE_ARRAY8_LITTLE
| CODE_DOUBLE_ARRAY32_BIG
| CODE_CODEPOINTER
| CODE_INFIXPOINTER
| CODE_CUSTOM
| CODE_BLOCK64
| CODE_SHARED64
| CODE_STRING64
| CODE_DOUBLE_ARRAY64_BIG
| CODE_DOUBLE_ARRAY64_LITTLE
| CODE_CUSTOM_LEN
| CODE_CUSTOM_FIXED

let code_max = 0x19

let magic_number = "\132\149\166\190"

type repr =
| RInt of int
| Rint64 of Int64.t
| RFloat64 of float
| RBlock of (int * int) (* tag × len *)
| RString of string
| RPointer of int
| RCode of int

type data =
| Int of int (* value *)
| Ptr of int (* pointer *)
| Atm of int (* tag *)
| Fun of int (* address *)

let input_byte (s, off) =
  let ans = Char.code (s.[!off]) in
  let () = incr off in
  ans

let input_binary_int chan =
  let i = input_byte chan in
  let j = input_byte chan in
  let k = input_byte chan in
  let l = input_byte chan in
  let ans = (i lsl 24) lor (j lsl 16) lor (k lsl 8) lor l in
  if i land 0x80 = 0
  then ans
  else ans lor ((-1) lsl 31)

let input_char chan = Char.chr (input_byte chan)
let input_string len chan = String.init len (fun _ -> input_char chan)

let parse_header chan =
  let magic = input_string 4 chan in
  let length = input_binary_int chan in
  let objects = input_binary_int chan in
  let size32 = input_binary_int chan in
  let size64 = input_binary_int chan in
  (magic, length, size32, size64, objects)

let input_int8s chan =
  let i = input_byte chan in
  if i land 0x80 = 0
    then i
    else i lor ((-1) lsl 8)

let input_int8u = input_byte

let input_int16s chan =
  let i = input_byte chan in
  let j = input_byte chan in
  let ans = (i lsl 8) lor j in
  if i land 0x80 = 0
    then ans
    else ans lor ((-1) lsl 16)

let input_int16u chan =
  let i = input_byte chan in
  let j = input_byte chan in
  (i lsl 8) lor j

let input_int32s chan =
  let i = input_byte chan in
  let j = input_byte chan in
  let k = input_byte chan in
  let l = input_byte chan in
  let ans = (i lsl 24) lor (j lsl 16) lor (k lsl 8) lor l in
  if i land 0x80 = 0
    then ans
    else ans lor ((-1) lsl 31)

let input_int32u chan =
  let i = input_byte chan in
  let j = input_byte chan in
  let k = input_byte chan in
  let l = input_byte chan in
  (i lsl 24) lor (j lsl 16) lor (k lsl 8) lor l

let input_int64s chan =
  let i = input_byte chan in
  let j = input_byte chan in
  let k = input_byte chan in
  let l = input_byte chan in
  let m = input_byte chan in
  let n = input_byte chan in
  let o = input_byte chan in
  let p = input_byte chan in
  let ans =
    (i lsl 56) lor (j lsl 48) lor (k lsl 40) lor (l lsl 32) lor
    (m lsl 24) lor (n lsl 16) lor (o lsl 8) lor p
  in
  if i land 0x80 = 0
    then ans
    else ans lor ((-1) lsl 63)

let input_int64u chan =
  let i = input_byte chan in
  let j = input_byte chan in
  let k = input_byte chan in
  let l = input_byte chan in
  let m = input_byte chan in
  let n = input_byte chan in
  let o = input_byte chan in
  let p = input_byte chan in
  (i lsl 56) lor (j lsl 48) lor (k lsl 40) lor (l lsl 32) lor
  (m lsl 24) lor (n lsl 16) lor (o lsl 8) lor p

let input_header32 chan =
  let i = input_byte chan in
  let j = input_byte chan in
  let k = input_byte chan in
  let l = input_byte chan in
  let tag = l in
  let len = (i lsl 14) lor (j lsl 6) lor (k lsr 2) in
  (tag, len)

let input_header64 chan =
  let i = input_byte chan in
  let j = input_byte chan in
  let k = input_byte chan in
  let l = input_byte chan in
  let m = input_byte chan in
  let n = input_byte chan in
  let o = input_byte chan in
  let p = input_byte chan in
  let tag = p in
  let len =
    (i lsl 46) lor (j lsl 38) lor (k lsl 30) lor (l lsl 22) lor
    (m lsl 14) lor (n lsl 6) lor (o lsr 2)
  in
  (tag, len)

let input_cstring chan : string =
  let buff = Buffer.create 17 in
  let rec loop () =
    match input_char chan with
    | '\o000' -> Buffer.contents buff
    | c -> Buffer.add_char buff c |> loop
  in loop ()

let input_intL chan : int64 =
  let i = input_byte chan in
  let j = input_byte chan in
  let k = input_byte chan in
  let l = input_byte chan in
  let m = input_byte chan in
  let n = input_byte chan in
  let o = input_byte chan in
  let p = input_byte chan in
  let ( lsl ) x y = Int64.(shift_left (of_int x) y) in
  let ( lor ) = Int64.logor in
  (i lsl 56) lor (j lsl 48) lor (k lsl 40) lor (l lsl 32) lor
  (m lsl 24) lor (n lsl 16) lor (o lsl 8) lor (Int64.of_int p)

let input_double_big chan : float =
  Int64.float_of_bits (input_intL chan)

let input_double_little chan : float =
  let i = input_byte chan in
  let j = input_byte chan in
  let k = input_byte chan in
  let l = input_byte chan in
  let m = input_byte chan in
  let n = input_byte chan in
  let o = input_byte chan in
  let p = input_byte chan in
  let ( lsl ) x y = Int64.(shift_left (of_int x) y) in
  let ( lor ) = Int64.logor in
  let bits =
    (p lsl 56) lor (o lsl 48) lor (n lsl 40) lor (m lsl 32) lor
    (l lsl 24) lor (k lsl 16) lor (j lsl 8) lor (Int64.of_int i) in
  Int64.float_of_bits bits

let parse_object chan =
  let data = input_byte chan in
  if prefix_small_block <= data then
    let tag = data land 0x0F in
    let len = (data lsr 4) land 0x07 in
    RBlock (tag, len)
  else if prefix_small_int <= data then
    RInt (data land 0x3F)
  else if prefix_small_string <= data then
    let len = data land 0x1F in
    RString (input_string len chan)
  else if data > code_max then
    assert false
  else match (Obj.magic data) with
  | CODE_INT8 ->
    RInt (input_int8s chan)
  | CODE_INT16 ->
    RInt (input_int16s chan)
  | CODE_INT32 ->
    RInt (input_int32s chan)
  | CODE_INT64 ->
    RInt (input_int64s chan)
  | CODE_SHARED8 ->
    RPointer (input_int8u chan)
  | CODE_SHARED16 ->
    RPointer (input_int16u chan)
  | CODE_SHARED32 ->
    RPointer (input_int32u chan)
  | CODE_BLOCK32 ->
    RBlock (input_header32 chan)
  | CODE_BLOCK64 ->
    RBlock (input_header64 chan)
  | CODE_STRING8 ->
    let len = input_int8u chan in
    RString (input_string len chan)
  | CODE_STRING32 ->
    let len = input_int32u chan in
    RString (input_string len chan)
  | CODE_CODEPOINTER ->
    let addr = input_int32u chan in
    for _i = 0 to 15 do ignore (input_byte chan); done;
    RCode addr
  | CODE_CUSTOM
  | CODE_CUSTOM_FIXED ->
    begin match input_cstring chan with
    | "_j" -> Rint64 (input_intL chan)
    | s -> Printf.eprintf "Unhandled custom code: %s" s; assert false
    end
  | CODE_DOUBLE_BIG ->
    RFloat64 (input_double_big chan)
  | CODE_DOUBLE_LITTLE ->
    RFloat64 (input_double_little chan)
  | CODE_DOUBLE_ARRAY32_LITTLE
  | CODE_DOUBLE_ARRAY8_BIG
  | CODE_DOUBLE_ARRAY8_LITTLE
  | CODE_DOUBLE_ARRAY32_BIG
  | CODE_INFIXPOINTER
  | CODE_SHARED64
  | CODE_STRING64
  | CODE_DOUBLE_ARRAY64_BIG
  | CODE_DOUBLE_ARRAY64_LITTLE
  | CODE_CUSTOM_LEN
    -> Printf.eprintf "Unhandled code %04x\n%!" data; assert false

let dbg = CDebug.create ~name:"constrsharing" ()

let parse chan =
  let chan = chan, ref 0 in
  let blocks = ref 0 in
  let steps = ref 0 in
  let (magic, _, _, _, size) = parse_header chan in
  let () = assert (magic = magic_number) in
  (* we only put blocks in memory as nothing else can contain constr
     however to be robust to possible untyped sharings
     (eg [Rel 1] with [QVar.Var 1])
     we do need to put every encountered block in memory
     even if by typing it shouldn't contain constrs.
  *)
  let memory = LargeArray.make size ((-1), [||]) in
  let current_object = ref 0 in

  let fill_obj = function
  | RPointer n ->
    let data = Ptr (!current_object - n) in
    data, None
  | RInt n ->
    let data = Int n in
    data, None
  | RString _ ->
    let data = Ptr !current_object in
    let () = incr current_object in
    data, None
  | RBlock (tag, 0) ->
    (* Atoms are never shared *)
    let data = Atm tag in
    data, None
  | RBlock (tag, len) ->
    incr blocks;
    let data = Ptr !current_object in
    let nblock = Array.make len (Atm (-1)) in
    let () = LargeArray.set memory !current_object (tag, nblock) in
    let () = incr current_object in
    data, Some nblock
  | RCode addr ->
    let data = Fun addr in
    data, None
  | Rint64 _ ->
    let data = Ptr !current_object in
    let () = incr current_object in
    data, None
  | RFloat64 _ ->
    let data = Ptr !current_object in
    let () = incr current_object in
    data, None
  in

  let rec fill block off accu =
    if Array.length block = off then
      match accu with
      | [] -> ()
      | (block, off) :: accu -> fill block off accu
    else
      let () = incr steps in
      let data, nobj = fill_obj (parse_object chan) in
      let () = block.(off) <- data in
      let block, off, accu = match nobj with
      | None -> block, succ off, accu
      | Some nblock -> nblock, 0, ((block, succ off) :: accu)
      in
      fill block off accu
  in
  let ans = [|Atm (-1)|] in
  let () = fill ans 0 [] in
  dbg Pp.(fun () ->
      v 0 (
        str "marshalled string size = " ++ int (String.length (fst chan)) ++ spc() ++
        str "repr steps = " ++ int !steps ++ spc() ++
        str "blocks = " ++ int !blocks ++ spc()
      ));
  (ans.(0), memory)

let repr v =
  let s = NewProfile.profile "constrsharing.marshal" (fun () -> Marshal.to_string v []) () in
  NewProfile.profile "constrsharing.repr" (fun () -> parse s) ()

type t = {
  uid : int;
  mutable refcount : int;
  self : Constr.t;
  kind : (t, t, Sorts.t, UVars.Instance.t, Sorts.relevance) Constr.kind_of_term;
}

let do_constr memory c data =
  let steps = ref 0 in
  let get_ptr = function
    | Int _ | Atm _ | Fun _ -> assert false
    | Ptr p -> LargeArray.get memory p
  in
  let get_ptr_vals ~tag ~len p =
    let tag', v = get_ptr p in
    assert (tag == tag' && len = Array.length v);
    v
  in
  let get_pair p =
    let v = get_ptr_vals ~tag:0 ~len:2 p in
    v.(0), v.(1)
  in
  let get_array_vals a p =
    let len = Array.length a in
    if len = 0 then [||]
    else get_ptr_vals ~tag:0 ~len p
  in
  let do_array do_elem a v =
    Array.map2 do_elem a (get_array_vals a v)
  in

  let seen = ref Int.Map.empty in

  let rec do_constr c = function
    | Int _ | Atm _ | Fun _ -> assert false
    | Ptr n ->
      match Int.Map.find_opt n !seen with
      | Some v -> v.refcount <- v.refcount + 1; v
      | None ->
        let () = incr steps in
        let tag, vals = LargeArray.get memory n in
        let k = fresh_constr ~tag vals c in
        let c = { uid = n; refcount = 1; self = c; kind = k } in
        seen := Int.Map.add n c !seen;
        c

  and fresh_constr ~tag vals c =
    let open Constr in
    let len = Array.length vals in
    match kind c with
    | Rel _ as k ->
      assert (tag = 0 && len = 1);
      k
    | Var _ as k ->
      assert (tag = 1 && len = 1);
      k
    | Meta _ as k ->
      assert (tag = 2 && len = 1);
      k
    | Evar ev -> (* "of pexistential", not "of evar * constr slist" *)
      assert (tag = 3 && len = 1);
      let evk, args = ev in
      let argsval = snd (get_pair vals.(0)) in
      Evar (evk, do_constr_slist args argsval)
    | Sort _ as k ->
      assert (tag = 4 && len = 1);
      k
    | Cast (c1,k,c2) ->
      assert (tag = 5 && len = 3);
      let c1 = do_constr c1 vals.(0) in
      let c2 = do_constr c2 vals.(2) in
      Cast (c1,k,c2)
    | Prod (na,c1,c2) ->
      assert (tag = 6 && len = 3);
      let c1 = do_constr c1 vals.(1) in
      let c2 = do_constr c2 vals.(2) in
      Prod (na,c1,c2)
    | Lambda (na,c1,c2) ->
      assert (tag = 7 && len = 3);
      let c1 = do_constr c1 vals.(1) in
      let c2 = do_constr c2 vals.(2) in
      Lambda (na,c1,c2)
    | LetIn (na,c1,c2,c3) ->
      assert (tag = 8 && len = 4);
      let c1 = do_constr c1 vals.(1) in
      let c2 = do_constr c2 vals.(2) in
      let c3 = do_constr c3 vals.(3) in
      LetIn (na,c1,c2,c3)
    | App (h,args) ->
      assert (tag = 9 && len = 2);
      let h = do_constr h vals.(0) in
      let args = do_constr_array args vals.(1) in
      App (h,args)
    | Const _ as k ->
      assert (tag = 10 && len = 1);
      k
    | Ind _ as k ->
      assert (tag = 11 && len = 1);
      k
    | Construct _ as k ->
      assert (tag = 12 && len = 1);
      k
    | Case (ci,u,pms,(p,r),iv,c,brs) ->
      assert (tag = 13 && len = 7);
      let pms = do_constr_array pms vals.(2) in
      let p = do_ctx p (fst @@ get_pair vals.(3)) in
      let iv = match iv with
        | NoInvert -> NoInvert
        | CaseInvert {indices=i} ->
          let ival = (get_ptr_vals ~tag:0 ~len:1 vals.(4)).(0)
          in
          let i = do_constr_array i ival in
          CaseInvert {indices=i}
      in
      let c = do_constr c vals.(5) in
      let brs = do_array do_ctx brs vals.(6) in
      Case (ci,u,pms,(p,r),iv,c,brs)
    | Fix (x,vrec) -> (* "of pfixpoint" *)
      assert (tag = 14 && len = 1);
      let vrec = do_recdef vrec (snd @@ get_pair vals.(0)) in
      Fix (x, vrec)
    | CoFix (x,vrec) -> (* "of cofixpoint" *)
      assert (tag = 15 && len = 1);
      let vrec = do_recdef vrec (snd @@ get_pair vals.(0)) in
      CoFix (x, vrec)
    | Proj (p,r,c) ->
      assert (tag = 16 && len = 3);
      let c = do_constr c vals.(2) in
      Proj (p,r,c)
    | Constr.Int _ as k ->
      assert (tag = 17 && len = 1);
      k
    | Float _ as k ->
      assert (tag = 18 && len = 1);
      k
    | Constr.String _ as k ->
      assert (tag = 19 && len = 1);
      k
    | Array (u,elems,def,ty) ->
      assert (tag = 20 && len = 4);
      let elems = do_constr_array elems vals.(1) in
      let def = do_constr def vals.(2) in
      let ty = do_constr ty vals.(3) in
      Array (u,elems,def,ty)

  and do_constr_array a v = do_array do_constr a v

  and do_constr_slist sl slval =
    let open SList in
    match sl, slval with
    | Nil, Int 0 -> SList.empty
    | Cons (c, tl), Ptr p ->
      let tag, vals = LargeArray.get memory p in
      assert (tag = 0 && Array.length vals = 2);
      let c = do_constr c vals.(0) in
      let tl = do_constr_slist tl vals.(1) in
      SList.cons c tl
    | Default (n, tl), Ptr p ->
      let tag, vals = LargeArray.get memory p in
      assert (tag = 1 && Array.length vals = 2);
      let tl = do_constr_slist tl vals.(1) in
      SList.defaultn n tl
    | (Nil | Cons _ | Default _), _ -> assert false

  and do_ctx (nas,c) v =
    let c = do_constr c (snd @@ get_pair v) in
    nas, c

  and do_recdef (nas,tys,bdys) v =
    let v = get_ptr_vals ~tag:0 ~len:3 v in
    let tys = Array.map2 do_constr tys (get_array_vals tys v.(1)) in
    let bdys = Array.map2 do_constr bdys (get_array_vals bdys v.(2)) in
    (nas, tys, bdys)

  in
  let c = do_constr c data in
  dbg Pp.(fun () -> str "do_constr steps = " ++ int !steps);
  c

let of_constr c =
  let data, memory = repr c in
  NewProfile.profile "constrsharing.do_constr" (fun () ->
      do_constr memory c data)
    ()

let uid x = x.uid
let refcount x = x.refcount
let self x = x.self
let kind x = x.kind
