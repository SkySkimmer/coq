(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

module MiniJson = struct
  type t =[
    | `Intlit of string
    | `String of string
    | `Assoc of (string * t) list
    | `List of t list
  ]

  (* https://www.ietf.org/rfc/rfc4627.txt *)
  (* https://github.com/ocaml-community/yojson/blob/4c1d4b52f9e87a4bd3b7f26111e8a4976c1e8676/lib/write.ml#L27 *)
  let prstring ch s =
    let buf = Buffer.create (String.length s) in
    let encode c =
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\b' -> Buffer.add_string buf "\\b"
      | '\012' -> Buffer.add_string buf "\\f"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | '\x00'..'\x1F'
      | '\x7F' ->
          Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
      | _ -> Buffer.add_char buf c
    in
    String.iter encode s;
    Format.fprintf ch "\"%s\"" (Buffer.contents buf)

  let rec pr ch : t -> _ = function
    | `String s -> prstring ch s
    | `Intlit s -> Format.fprintf ch "%s" s
    | `Assoc elts ->
      Format.fprintf ch "{ %a }" prrecord elts
    | `List elts ->
      Format.fprintf ch "[\n%a\n]" prarray elts

  and prrecord ch = function
    | [] -> ()
    | [(x,v)] -> Format.fprintf ch "%a: %a" prstring x pr v
    | (x,v) :: l -> Format.fprintf ch "%a: %a, %a" prstring x pr v prrecord l

  and prarray ch = function
    | [] -> ()
    | [x] -> pr ch x
    | x :: l -> Format.fprintf ch "%a,\n%a" pr x prarray l


  let pid = Unix.getpid()

  let pids = string_of_int pid
  let base = [("pid", `Intlit pids); ("tid", `Intlit pids)]

  let duration ~name ~ph ~ts ?args () =
    let l = ("name", `String name) :: ("ph", `String ph) :: ("ts", `Intlit ts) :: base in
    let l = match args with
      | None -> l
      | Some args -> ("args", `Assoc args) :: l
    in
    `Assoc l
end

type accu = {
  ch : Format.formatter;
  mutable sums : (float * (float * int) CString.Map.t) list;
}

let accu = ref None

let is_profiling () = Option.has_some !accu

let f fmt = match !accu with
  | None -> assert false
  | Some { ch } -> Format.fprintf ch fmt

let gettime = Unix.gettimeofday

let gettimeopt = function
  | Some t -> t
  | None -> gettime()

let prtime t =
  Format.sprintf "%.0f" (t *. 1E6)

module Counters = struct

  (* time is handled separately because it has special status in the output format *)
  type t = {
    major_words : float;
    minor_words : float;
    major_collections : int;
    minor_collections : int;
    minor_time : int64 (* nanoseconds *);
    major_time : int64 (* nanoseconds *);
    instr : System.instruction_count;
  }

  let current_minor_time = ref Int64.zero
  let current_major_time = ref Int64.zero

  let get () =
    let gc = Gc.quick_stat() in
    {
      major_words = gc.major_words;
      minor_words = gc.minor_words;
      major_collections = gc.major_collections;
      minor_collections = gc.minor_collections;
      minor_time = !current_minor_time;
      major_time = !current_major_time;
      instr = Instr.read_counter();
    }

  let global_start = get ()

  let (-) b a = {
    major_words = b.major_words -. a.major_words;
    minor_words = b.minor_words -. a.minor_words;
    major_collections = b.major_collections - a.major_collections;
    minor_collections = b.minor_collections - a.minor_collections;
    minor_time = Int64.sub b.minor_time a.minor_time;
    major_time = Int64.sub b.major_time a.major_time;
    instr = System.instructions_between ~c_start:a.instr ~c_end:b.instr;
  }

  let format x =
    let ppf tdiff = `String (Format.sprintf "%.3G w" tdiff) in
    let ppi i = `Intlit (string_of_int i) in
    let ppi64 i = `Intlit (Int64.to_string i) in
    let instr = match x.instr with
      | Ok count -> [("instr", ppi64 count)]
      | Error _ -> []
    in
    (* don't print measurements equal to 0 (instr is never going to be 0) *)
    let cons trivial v l = if not trivial then v :: l else l in
    cons (Float.equal x.major_words 0.) ("major_words",ppf x.major_words) @@
    cons (Float.equal x.minor_words 0.) ("minor_words", ppf x.minor_words) @@
    cons (Int.equal x.major_collections 0) ("major_collect", ppi x.major_collections) @@
    cons (Int.equal x.minor_collections 0) ("minor_collect", ppi x.minor_collections) @@
    cons (Int64.equal x.minor_time Int64.zero) ("minor_time", ppi64 x.minor_time) @@
    cons (Int64.equal x.major_time Int64.zero) ("major_time", ppi64 x.major_time) @@
    instr

  let make_diffs ~start ~stop = format (stop - start)

end

let global_start_time = gettime ()

let duration ~time name ph ?args ?(last=",") () =
  f "%a%s\n" MiniJson.pr (MiniJson.duration ~name ~ph ~ts:(prtime time) ?args ()) last

let enter_sums ?time () =
  let accu = Option.get !accu in
  let time = gettimeopt time in
  accu.sums <- (time, CString.Map.empty) :: accu.sums

let enter ?time name ?args () =
  let time = gettimeopt time in
  enter_sums ~time ();
  duration ~time name "B" ?args ()

let poll, init_poll =
  let f = ref (fun () -> ()) in
  let init () =
    Runtime_events.start();

    let cursor = Runtime_events.create_cursor None in

    let callbacks =
      let open Runtime_events in
      (* do we actually need a stack or is a start_of_current_phase ref enough? *)
      let current_minor = ref [] in
      let current_major = ref [] in
      let runtime_begin _ ts = function
        | EV_MINOR -> current_minor := ts :: !current_minor
        | EV_MAJOR -> current_major := ts :: !current_major
        | _ -> ()
      in
      let update ref stack ts =
        match !stack with
        | [] -> assert false
        | start :: rest ->
          stack := rest;
          ref := Int64.(add !ref (sub (Timestamp.to_int64 ts) (Timestamp.to_int64 start)))
      in
      let runtime_end _ ts = function
        | EV_MINOR -> update Counters.current_minor_time current_minor ts
        | EV_MAJOR -> update Counters.current_major_time current_major ts
        | _ -> ()
      in
      let lost_events start stop =
        (* not sure what start/stop mean or what's the best way to report these *)
        Printf.eprintf "lost runtime events %d %d\n" start stop
      in
      Callbacks.create ~runtime_begin ~runtime_end ~lost_events ()
    in

    f := (fun () -> ignore (Runtime_events.read_poll cursor callbacks None : int))
  in
  (fun () -> !f()), init

let leave_sums ?time name () =
  let accu = Option.get !accu in
  let time = gettimeopt time in
  (* polling here should be frequent enough to avoid losing events (hopefully) *)
  let () = poll() in
  match accu.sums with
  | [] -> assert false
  | [start,sum] -> accu.sums <- []; sum, time -. start
  | (start, sum) :: (start', next) :: rest ->
    let dur = time -. start in
    let next = CString.Map.update name (function
        | None -> Some (dur, 1)
        | Some (dur', cnt) -> Some (dur +. dur', cnt+1))
        next
    in
    let next = CString.Map.union (fun name' (l,cnt) (r,cnt') ->
        if String.equal name name' then Some (r,cnt+cnt')
        else Some (l +. r, cnt+cnt'))
        sum next
    in
   accu. sums <- (start', next) :: rest;
    sum, dur

let leave ?time name ?(args=[]) ?last () =
  let time = gettimeopt time in
  let sum, dur = leave_sums ~time name () in
  let sum = List.map (fun (name, (t, cnt)) ->
      name, `String
        (Format.sprintf "%.3G us, %d %s" (t *. 1E6) cnt (CString.plural cnt "call")))
      (CString.Map.bindings sum)
  in
  let args = ("subtimes", `Assoc sum) :: args in
  duration ~time name "E" ~args ?last ()

(* NB: "process" and "init" are unconditional because they don't go
   through [profile] and I'm too lazy to make them conditional *)
let components =
  match Sys.getenv_opt "COQ_PROFILE_COMPONENTS" with
  | None -> CString.Pred.(full |> remove "unification" |> remove "Conversion")
  | Some s ->
    List.fold_left (fun cs c -> CString.Pred.add c cs)
      CString.Pred.empty
      (String.split_on_char ',' s)

let profile name ?args f () =
  if not (is_profiling ()) then f ()
  else if CString.Pred.mem name components then begin
    let args = Option.map (fun f -> f()) args in
    enter name ?args ();
    let start = Counters.get () in
    let v = try f ()
      with e ->
        let e = Exninfo.capture e in
        let args = Counters.make_diffs ~start ~stop:(Counters.get()) in
        leave name ~args ();
        Exninfo.iraise e
    in
    let args = Counters.make_diffs ~start ~stop:(Counters.get()) in
    leave name ~args ();
    v
  end
  else begin
    enter_sums ();
    let v = try f ()
      with e ->
        let e = Exninfo.capture e in
        ignore (leave_sums name () : _ * _);
        Exninfo.iraise e
    in
    ignore (leave_sums name () : _ * _);
    v
  end

type settings =
  { output : Format.formatter
  }

let init { output } =
  let () = assert (not (is_profiling())) in
  accu := Some { ch = output; sums = [] };
  init_poll ();
  f "{ \"traceEvents\": [\n";
  enter ~time:global_start_time "process" ();
  enter ~time:global_start_time "init" ();
  let args = Counters.(make_diffs ~start:global_start ~stop:(get())) in
  leave "init" ~args ()

let pause () =
  let v = !accu in
  accu := None;
  v

let resume v =
  assert (not (is_profiling()));
  accu := Some v

let finish () = match !accu with
  | None -> assert false
  | Some { ch } ->
    let args = Counters.(make_diffs ~start:global_start ~stop:(get())) in
    leave "process" ~last:"" ~args ();
    Format.fprintf ch "],\n\"displayTimeUnit\": \"us\" }";
    accu := None
