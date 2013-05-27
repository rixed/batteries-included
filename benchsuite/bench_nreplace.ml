(* Run with:
     make bench BENCH_TARGETS=benchsuite/bench_nreplace.native
 *)
open Batteries
open String

(*************************************************************
 * find benchmarks
 *************************************************************)

let find_horspool ~sub =
  let sublen = String.length sub in

  (* initialize shifting table. improved horspool - all elements are >=1
   * Abuse string as uint8 array. *)
  let shift = String.make 256 (char_of_int (min 255 sublen)) in
  for i = max 0 (sublen - 255) to sublen - 1 do
    String.unsafe_set shift
      (int_of_char (String.unsafe_get sub i))
      (char_of_int (sublen - i))
  done;

  (* allow early initialization on partial binding *)
  fun ~str ~pos ->
    let strlen = String.length str in
    let rec worker i =
      if i+sublen >= strlen then raise Not_found;
      (* shift according to shifting table - will shift >=1 *)
      let i =
          i + int_of_char (String.unsafe_get shift
                (int_of_char (BatString.unsafe_get str (i+sublen))))
      in
      if i+sublen > strlen then raise Not_found;
      let j = ref 0 in
      (* compare - since order is not important unroll the loop a bit
       * by doing two comparisons at once, starting at both ends. *)
      while
        BatString.unsafe_get str (i + !j) = BatString.unsafe_get sub !j &&
        BatString.unsafe_get str (i + sublen - 1 - !j) = BatString.unsafe_get sub (sublen - 1 - !j) &&
        (incr j; 2 * !j < sublen)
      do () done;
      (* all equal ? *)
      if 2 * !j >= sublen
      then i
      else worker i
    in
    if pos < 0 || pos > strlen then raise (Invalid_argument "String.find_from");
    if sublen = 0 then pos
    else worker (pos-1)

let find_simple ~sub ~str ~pos = String.find_from str pos sub

let find_adaptive ~sub ~str =
  let strlen = String.length str
  and sublen = String.length sub in
  if (sublen - 2) * strlen < 190 * sublen
  then find_simple ~sub ~str
  else find_horspool ~sub ~str

let find_all ~sub ?(pos=0) str =
  let worker = find_horspool ~sub in
  let nexti = ref pos in
  BatEnum.from begin fun () ->
    let i =
      try worker ~pos:!nexti ~str with
      Not_found -> raise Enum.No_more_elements
    in
    nexti := i+1;
    i
  end


(*************************************************************
 * replace benchmarks
 *************************************************************)

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

(* Independently, MadRoach implemented the same idea with less luck aparently *)
let nreplace_madroach ~str ~sub ~by =
  let strlen = String.length str
  and sublen = String.length sub
  and bylen  = String.length by in
  let dlen = bylen - sublen in

  (* collect all positions where we need to replace,
   * skipping overlapping occurences *)
  let find = find_adaptive ~sub ~str in
  let rec loop todo dstlen i =
    try
      let i' = find i in
      loop (i'::todo) (dstlen + dlen) (i'+1)
    with
    Not_found -> todo, dstlen
  in
  let todo, dstlen = loop [] strlen 0 in

  (* blit to new destination string *)
  let dst = String.create dstlen in
  let rec replace li delta = function
    |i::todo ->
        let delta = delta - dlen in
        let dsti = i + delta in
        String.unsafe_blit str (i+sublen) dst (dsti+bylen) (li-i-sublen);
        String.unsafe_blit by 0 dst dsti bylen;
        replace i delta todo
    |[] ->
        String.unsafe_blit str 0 dst 0 li;
        li, delta
  in
  let li, delta = replace strlen (dstlen-strlen) todo in
  assert (delta = 0);
  String.blit str 0 dst 0 li;
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
    if !pos < 0 then raise BatEnum.No_more_elements else
    try
      let next_pos = BatString.find_from str !pos pat in
      let sub = BatSubstring.unsafe_substring str !pos (next_pos - !pos) in
      pos := next_pos + pat_len;
      sub
    with Not_found ->
      let sub = BatSubstring.extract str !pos None in
      pos := -1 ;
      sub
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

let long_text =
  File.lines_of "benchsuite/bench.ml"
  |> Enum.cycle ~times:100 |> List.of_enum |> concat ""

let find_bench_for_len name strlen sub =
  let str = String.sub long_text 0 strlen in
  let run (f :sub:string -> str:string -> pos:int -> int) iters =
    for i=1 to iters do
      let f' = f ~sub ~str in
      try
        let rec loop i =
          loop (f' ~pos:i + 1)
        in
        loop 0
      with Not_found -> ()
    done
  in
  print_newline ();
  Bench.bench_n [
    "simple "^ name, run find_simple;
    "horspool "^ name, run find_horspool;
    "adaptive "^ name, run find_adaptive;
  ] |>
  Bench.run_outputs
;;

let replace_bench_for_len length name =
  let run rep iters =
    for i=1 to iters do
      (* "realistic" workload that attempts to exercise all interesting cases *)
      let str = sub long_text 0 length in
      let str = rep ~str ~sub:"let" ~by:"let there be light" in
      let str = rep ~str ~sub:"nreplace" ~by:"nr" in
      let str = rep ~str ~sub:"you wont find me" ~by:"" in
      let str = rep ~str ~sub:"match" ~by:"match" in
      let str = rep ~str ~sub:" " ~by:"  " in
      ignore str
    done
  in

  Bench.bench_n [
    "orig "^ name, run nreplace_orig ;
    "glyn "^ name, run nreplace_glyn ;
    "rxd "^ name, run nreplace_rxd ;
    "thelema "^ name, run nreplace_thelema ;
    "thelema2 "^ name, run nreplace_thelema2 ;
    "madroach "^ name, run nreplace_madroach ;
    "gasche simple "^ name, run nreplace_substring_simple ;
    "gasche enum "^ name, run nreplace_substring_enum ;
    "gasche optimized "^ name, run nreplace_substring_optimized ;
  ] |>
  Bench.run_outputs

let main =
    (* First check that all implementation performs superficialy the same *)
    let check ~str ~sub ~by =
        let outp = nreplace_orig ~str ~sub ~by in
        List.iter (fun (d,rep) ->
            let outp' = rep ~str ~sub ~by in
            if outp' <> outp then (
                Printf.fprintf stderr "Implementation %s failed for str:%S, sub:%S, by:%S got %S instead of %S\n"
                    d str sub by outp' outp ;
                exit 1
            )) [
            "glyn", nreplace_glyn ;
            "rxd", nreplace_rxd ;
            "thelema", nreplace_thelema ;
            "thelema2", nreplace_thelema2 ;
            "madroach", nreplace_madroach ;
            "gasche simple", nreplace_substring_simple ;
            "gasche enum", nreplace_substring_enum ;
            "gasche optimz", nreplace_substring_optimized
        ] in
    check ~str:"foo bar baz" ~sub:"bar" ~by:"BAR" ;
    check ~str:"foo bar baz" ~sub:"bar" ~by:"" ;
    check ~str:"foo bar baz" ~sub:"a" ~by:"BAR" ;
    check ~str:"foo bar baz" ~sub:" " ~by:"   " ;

    print_endline "\nBenchmarking find:\n=============================================\n";

    let vary_length sub =
      let sublen = string_of_int (String.length sub) in
      Array.iter begin fun l ->
        find_bench_for_len (sublen ^ " in " ^ string_of_int l ^ "bytes") l sub;
        print_endline "-----------------------------------------------------------";
      end
      [|10; 100; 150; 200; 500; 1_000; 10_000|]
    in
    vary_length " ";
(*     vary_length "le"; *)
    vary_length "let";
(*
    vary_length "matc";
    vary_length "match";
*)
    vary_length "match ";
    vary_length "let resample ests num_resamples samples ";

    print_endline "\nBenchmarking nreplace:\n=======================================\n";

    replace_bench_for_len 10 "10bytes" ;
    print_endline "\n-------------------------------";
    replace_bench_for_len 100 "100bytes" ;
    print_endline "\n-------------------------------";
    replace_bench_for_len 1000 "1kb" ;
    print_endline "\n-------------------------------";
    replace_bench_for_len 10_000 "10kb";
(*
    print_endline "\n-------------------------------";
    do_bench_for_len 100_000 "100kb";
    print_endline "\n-------------------------------";
    do_bench_for_len 1_000_000 "1mb" ;
*)
  ()
