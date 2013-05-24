(* Run with:
     make bench BENCH_TARGETS=benchsuite/bench_nreplace.native
 *)
open Batteries
open String

(* The original Batteries String.nreplace *)
let nreplace_orig ~str ~sub ~by =
  if sub = "" then invalid_arg "nreplace: cannot replace all empty substrings" ;
  let parts = nsplit str ~by:sub in
  String.concat by parts

(* The suggestion from Glyn Webster that started it all.
   Notice that it replaces substrings from left to right instead of right to left. *)
let nreplace_glyn ~str ~sub ~by =
   if sub = "" then invalid_arg "nreplace: cannot replace all empty substrings" ;
   let find_sub pos = try find_from str pos sub with Not_found -> -1 in  
   (* allows loop to be tail recursive *)
   let sublen = length sub in
   let strlen = length str in
   let buffer = Buffer.create strlen in
   let rec loop curpos =
     if curpos = strlen then
       Buffer.contents buffer
     else
       let subpos = find_sub curpos in
       if subpos = -1 then
         ( Buffer.add_substring buffer str curpos (strlen - curpos) ;
           Buffer.contents buffer )
       else
         ( Buffer.add_substring buffer str curpos (subpos - curpos) ;
           Buffer.add_string buffer by ;
           loop (subpos + sublen) )
   in
   loop 0

(* Then Thelema suggested preallocating the final string. Here is a first
 * implementation, performing two rfind_from which is apparently a very bad idea *)
let nreplace_rxd ~str ~sub ~by =
  if sub = "" then invalid_arg "nreplace: cannot replace all empty substrings" ;
  let strlen = length str in
  let sublen = length sub in
  let bylen  = length by in
  let dlen   = bylen - sublen in
  let rec loop_subst l i =
    match (try Some (rfind_from str (i-1) sub) with Not_found -> None) with
    | None -> l
    | Some i' -> loop_subst (l + dlen) i' in
  let newlen =
    if dlen = 0 then strlen else loop_subst strlen strlen in
  let newstr = create newlen in
  let rec loop_copy i j =
    match (try Some (rfind_from str (i-1) sub) with Not_found -> None) with
    | None ->
      (* still need the first chunk *)
      blit str 0 newstr 0 i
    | Some i' ->
      let j' = j - (i - i') - dlen in
      (* newstring.[j .. end] is already inited. Init from j' to (j-1). *)
      blit by 0 newstr j' bylen ;
      blit str (i'+sublen) newstr (j'+bylen) (i-i'-sublen) ;
      loop_copy i' j' in
  loop_copy strlen newlen ;
  newstr

(* So Thelema proposed a version without the double rfind_from
 * (taken from https://gist.github.com/thelema/5639270 + small fix) *)
let nreplace_thelema ~str ~sub ~by =
  if sub = "" then invalid_arg "nreplace: cannot replace all empty substrings" ;
  let strlen = length str in
  let sublen = length sub in
  let bylen = length by in
  let dlen = bylen - sublen in
  let rec loop_subst idxes i =
    match Exceptionless.rfind_from str (i-1) sub with
    | None -> idxes
    | Some i' -> loop_subst (i'::idxes) i' in
  let idxes = loop_subst [] strlen in
  let newlen = strlen + List.length idxes * dlen in
  let newstr = create newlen in
  let rec loop_copy i j idxes =
    match idxes with
    | [] ->
      (* still need the last chunk *)
      blit str i newstr j (strlen-i)
    | i'::rest ->
      let di = i' - i in
      blit str i newstr j di ;
      blit by 0 newstr (j + di) bylen ;
      loop_copy (i + di + sublen) (j + di + bylen) rest in
    loop_copy 0 0 idxes ;
    newstr

(* Same as above but avoiding the List.length *)
let nreplace_thelema2 ~str ~sub ~by =
  if sub = "" then invalid_arg "nreplace: cannot replace all empty substrings" ;
  let strlen = length str in
  let sublen = length sub in
  let bylen = length by in
  let dlen = bylen - sublen in
  let rec loop_subst idxes newlen i =
    match (try rfind_from str (i-1) sub with Not_found -> -1) with
    | -1 -> idxes, newlen
    | i' -> loop_subst (i'::idxes) (newlen+dlen) i' in
  let idxes, newlen = loop_subst [] strlen strlen in
  let newstr = create newlen in
  let rec loop_copy i j idxes =
    match idxes with
    | [] ->
      (* still need the last chunk *)
      blit str i newstr j (strlen-i)
    | i'::rest ->
      let di = i' - i in
      blit str i newstr j di ;
      blit by 0 newstr (j + di) bylen ;
      loop_copy (i + di + sublen) (j + di + bylen) rest in
    loop_copy 0 0 idxes ;
    newstr

(* Independantly, MadRoach implemented the same idea with less luck aparently *)
let nreplace_madroach ~str ~sub ~by =
  let strlen = String.length str
  and sublen = String.length sub
  and bylen  = String.length by in

  (* compare to sub at position i in str *)
  let compare i =
    let rec compare' j =
      if j >= sublen then true
      else if i+j >= strlen || str.[i+j] <> sub.[j] then false
      else compare' (j+1)
    in
    compare' 0
  in
  (* collect all positions where we need to replace *)
  if sublen = 0 then invalid_arg "nreplace: empty sub not allowed";
  let rec collect todo i =
    if i >= strlen then todo
    else if compare i
    then collect (i::todo) (i+sublen)
    else collect todo (i+1)
  in
  let todo = collect [] 0 in

  (* create destination string *)
  let dst = String.create (strlen + List.length todo * (bylen - sublen)) in

  (* do the replacement *)
  let srci, dsti =
    (* todo is in reverse order, therefore fold right. *)
    List.fold_right
      begin fun i (srci,dsti) ->
        let skiplen = i-srci in
        String.blit str srci dst dsti skiplen;
        String.blit by 0 dst (dsti+skiplen) bylen;
        (srci+skiplen+sublen, dsti+skiplen+bylen)
      end
      todo
      (0,0)
  in
  assert (strlen - srci = String.length dst - dsti);
  String.blit str srci dst dsti (strlen - srci);
  dst

(* Gasche had its own idea based on substrings.
   Here are several versions, any of which seams faster than all the above.
   See:
   https://github.com/ocaml-batteries-team/batteries-included/pull/372#issuecomment-18399379
   for a discussion.*)

(* should be BatSubstring.nsplit *)
let nsplit str pat =
  let pat_len = String.length pat in
  let rec loop pos rev_subs =
    let next_pos =
      try BatString.find_from str pos pat
      with Not_found -> -1
    in
    if next_pos = -1 then
      (BatSubstring.extract str pos None :: rev_subs)
    else
      let sub = BatSubstring.unsafe_substring str pos (next_pos - pos) in
      loop (next_pos + pat_len) (sub :: rev_subs)
  in
  List.rev (loop 0 [])

(* should be BatSubstring.nsplit_enum *)
let nsplit_enum str pat =
  let pat_len = String.length pat in
  let pos = ref 0 in
  BatEnum.from (fun () ->
    try
      let next_pos = BatString.find_from str !pos pat in
      let sub = BatSubstring.unsafe_substring str !pos (next_pos - !pos) in
      pos := next_pos + pat_len;
      sub
    with Not_found -> raise BatEnum.No_more_elements
  )

(* should be BatSubstring.concat, with a separator argument *)
let concat_optimized ~sep ssl =
  let sep_len = String.length sep in
  (* use of Obj.magic is unfortunate here, but it would not be present
     if this function was implemented inside BatSubstring. Another
     option would be to make BatSubstring.t a [private (string * int
     * int)] and use a case here, but I'm not sure it's wise to expose
     the representation publicly -- we may want to change, say, from
     (string * start_pos * len) to (string * start_pos * end_pos). *)
  let ssl : (string * int * int) list = Obj.magic (ssl : BatSubstring.t list) in
  match ssl with
    | [] -> ""
    | (s,o,len)::tl ->
      let total_len =
        let rec count acc = function
          | [] -> acc
          | (_,_,l)::tl -> count (acc + sep_len + l) tl
        in count len tl
      in
      let item = String.create total_len in
      String.unsafe_blit s o item 0 len;
      let pos = ref len in
      let rec loop = function
        | [] -> ()
        | (s,o,len)::tl ->
          String.unsafe_blit sep 0 item !pos sep_len;
          pos := !pos + sep_len;
          String.unsafe_blit s o item !pos len;
          pos := !pos + len;
          loop tl;
      in loop tl;
      item

(* should be BatSubstring.concat, with a separator argument *)
let concat_simple ~sep ssl =  
  let sep_len = String.length sep in
  (* see comment above about Obj.magic *)
  let ssl : (string * int * int) list = Obj.magic (ssl : BatSubstring.t list) in
  match ssl with
    | [] -> ""
    | (s,o,len)::tl ->
      let total_len = List.fold_left (fun acc (_,_,l) -> acc+sep_len+l) len tl in
      let item = String.create total_len in
      String.unsafe_blit s o item 0 len;
      let pos = ref len in
      let write (s,o,len) =
        String.unsafe_blit sep 0 item !pos sep_len;
        pos := !pos + sep_len;
        String.unsafe_blit s o item !pos len;
        pos := !pos + len;
      in
      List.iter write tl;
      item

let concat_enum ~sep enum =
  match BatEnum.get enum with
    | None -> ""
    | Some hd ->
      let buf = Buffer.create 100 in
      Buffer.add_string buf (BatSubstring.to_string hd);
      BatEnum.iter (fun substr ->
        (* see comment above about Obj.magic *)
        let (s,o,l) = (Obj.magic (substr : BatSubstring.t) : string * int * int) in
        Buffer.add_string buf sep;
        Buffer.add_substring buf s o l;
      ) enum;
      Buffer.contents buf

let nreplace_substring_simple ~str ~sub ~by =
  concat_simple ~sep:by (nsplit str sub)

let nreplace_substring_optimized ~str ~sub ~by =
  concat_optimized ~sep:by (nsplit str sub)

let nreplace_substring_enum ~str ~sub ~by =
  concat_enum ~sep:by (nsplit_enum str sub)

(* We tests these nreplace implementations on this very file, substituting various
 * realistic words by others. *)

let lines =
  File.lines_of "benchsuite/bench_nreplace.ml" |>
  Array.of_enum

let make_long_lines mult =
  Array.map (fun s ->
    let len = String.length s in
    String.init (len * mult) (fun i -> s.[i mod len]))
    lines

let run rep mult =
  make_long_lines mult |>
  Array.iter (fun str ->
    (* "realistic" workload that attempts to exercise all interesting cases *)
    let str = rep ~str ~sub:"let" ~by:"let there be light" in
    let str = rep ~str ~sub:"nreplace" ~by:"nr" in
    let str = rep ~str ~sub:"you wont find me" ~by:"" in
    let str = rep ~str ~sub:"match" ~by:"match" in
    let str = rep ~str ~sub:" " ~by:"  " in
    ignore str)

let do_bench_for_len mult name =
  Bench.bench_funs [
    "orig "^ name, run nreplace_orig ;
    "glyn "^ name, run nreplace_glyn ;
    "rxd "^ name, run nreplace_rxd ;
    "thelema "^ name, run nreplace_thelema ;
    "thelema2 "^ name, run nreplace_thelema2 ;
    "madroach "^ name, run nreplace_madroach ;
    "gasche simple "^ name, run nreplace_substring_simple ;
    "gasche enum "^ name, run nreplace_substring_enum ;
    "gasche optimized "^ name, run nreplace_substring_optimized ;
  ] mult |>
  Bench.run_outputs

let main =
    do_bench_for_len 1 "short" ;
    do_bench_for_len 10 "long" ;
    do_bench_for_len 100 "very long"

