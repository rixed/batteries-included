(* 
 * BatBench - Benchmarking functions
 * Copyright (C) 2011 Edgar Friendly
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** Benchmarking functions, based on haskell criterion
   
   @author Edgar Friendly <thelema314@gmail.com>
*)

open Printf
open BatStd

(* TODO: make customizable timer? *)
let timer = Sys.time

let rec repeat n f x = if n <= 0 then () else (ignore (f x); repeat (n-1) f x)

let time f x = 
  let t0 = timer () in
  let ret = f x in
  timer () -. t0, ret

let time_ f x = 
  let t0 = timer () in
  f x;
  timer () -. t0

let (/^) a b = (float a) /. (float b)

let rec print_time oc t =
  if t < 0. then fprintf oc "-%a" print_time (-. t)
  else if t >= 1.   then fprintf oc "%f s" t
  else if t >= 1e-3 then fprintf oc "%f ms" (t*.1e3)
  else if t >= 1e-6 then fprintf oc "%f us" (t*.1e6)
  else if t >= 1e-9 then fprintf oc "%f ns" (t*.1e9)
  else if t >= 1e-12 then fprintf oc "%f ps" (t*.1e12)
  else fprintf oc "%g s" t

let run_for_time t f seed0 = 
  let t0 = timer () in
  let rec loop seed iters =
    let now = timer() in
    if now -. t0 > t *. 10. then 
      failwith (sprintf "Took too long to run: seed %d iters %d" seed iters);
    let ti, ret = time f seed in
    if ti > t then (ti, seed, ret)
    else loop (2*seed) (iters+1)
  in
  loop seed0 0

let mean a = BatArray.reduce (+.) a /. float (Array.length a)

type outliers = {
  data_count:  int;
  low_severe:  int; ls_limit: float;
  low_mild:    int; lm_limit: float;
  high_mild:   int; hm_limit: float;
  high_severe: int; hs_limit: float;
}

let print_outliers oc {data_count=dc; low_severe=ls; low_mild=lm; high_mild=hm; high_severe=hs} =
  let print cat thr n =
    if n > thr then
      fprintf oc "  %d (%.1f%%) %s\n" n (n/^dc *. 100.) cat
  in
  let one_percent = dc / 100 in
  print "Low Severe" 0 ls;
  print "Low Mild" one_percent lm;
  print "High Mild" one_percent hm;
  print "High Severe" 0 hs

(* Samples must be sorted in increasing order *)
let quantile nth quantiles samples =
  assert (quantiles >= 2);
  assert (nth >= 0 && nth <= quantiles);
  let n = Array.length samples in
  let idx_float = ((n-1) * nth) /^ quantiles in
  let idx = int_of_float (idx_float) in
  let interp = idx_float -. (float idx) in
  (* weighted average of idx'th and (idx+1)'th sample *)
  samples.(idx) +. interp *. (samples.(idx+1) -. samples.(idx))

(* searches a sorted array for the offset of the transition between
   elements less than elem and those greater than or equal to elem *)
let find_transition (data:float array) elem =
  let rec iter a b = 
    (* the bounds of the search includes data.(a) and excludes data.(b) *)
    if a = b then a
    else
      let mid = a + (b - a)/2 in
      match data.(mid) with
        | value when value = elem -> mid
        | value when value < elem -> iter (mid + 1) b
        | _                       -> iter a mid
  in
  iter 0 (Array.length data)


let analyze_mean _i a = 
  let m = mean a in 
  let len = Array.length a in
  let sorted = Array.copy a in
  Array.sort (BatFloat.compare) sorted;
  let q1 = quantile 1 4 sorted in
  let q3 = quantile 3 4 sorted in
  let inter_quartile_range = q3 -. q1 in
  if inter_quartile_range <> 0. then (
    let sevr_lo = q1 -. inter_quartile_range *. 3. in
    let mild_lo = q1 -. inter_quartile_range *. 1.5 in
    let mild_hi = q3 +. inter_quartile_range *. 1.5 in
    let sevr_hi = q3 +. inter_quartile_range *. 3. in
    let slo_pos = find_transition sorted sevr_lo in
    let mlo_pos = find_transition sorted mild_lo in
    let mhi_pos = find_transition sorted mild_hi in
    let shi_pos = find_transition sorted sevr_hi in
    {data_count = len; 
     low_severe = slo_pos;  ls_limit = sevr_lo;
     low_mild = mlo_pos - slo_pos; lm_limit = mild_lo;
     high_mild = shi_pos - mhi_pos; hm_limit = mild_hi;
     high_severe = len-shi_pos; hs_limit = sevr_hi;
    } 
    |> print_outliers stdout;
  );
  printf "N: %d Inter-quartile range = %a, min=%a, max=%a\n" 
    len
    print_time inter_quartile_range 
    print_time sorted.(0) 
    print_time sorted.(len-1);
  m

type effect = 
  | Unaffected (* less then 1% effect *)
  | Slight     (* between 1% and 10% *)
  | Moderate   (* between 10% and 50% *)
  | Severe     (* more than 50% *)

let effect_of_var x = 
  if x < 0.01 then Unaffected
  else if x < 0.1 then Slight
  else if x < 0.5 then Moderate
  else Severe

let outlier_variance mu sigma n =
  let n_fl = float n in
  let ua = mu /. n_fl in
  let sb2 = sigma *. sigma in
  let sg = min (ua /. 8.) (sigma /. sqrt n_fl) in
  let sg2 = sg *. sg in
  let cmax x = 
    let d = 2. *. (ua -. x) in
    let ad = n_fl *. d in
    let k0 = -. n_fl *. ad in
    let k1 = sb2 -. n_fl *. sg2 +. ad in
    let det = k1 *. k1 -. 4. *. sg2 *. k0 in
    floor (-2. *. k0 /. (k1 +. sqrt det)) |> int_of_float
  in
  let var_out c = let ac = n-c in (ac /^ n) *. (sb2 -. float ac *. sg2) in
  let minby f x v = min (f x) (f v) in
  let var_out_min = minby var_out 1 (minby cmax 0. (ua /. 2.)) in
  var_out_min, effect_of_var var_out_min

type environment = {clock_res: float; clock_cost: float}

let is_positive x = x > 0.
let get_environment () =
  let resolution i = 
    let times = Array.init (i+1) (fun _ -> timer()) in
    let pos_diffs = 
      Array.init i (fun i -> times.(i+1) -. times.(i)) 
      |> BatArray.filter is_positive (* FIXME: include zeros? *)
    in
    pos_diffs
  in
  let cost t t0 = 
    let tclock i = time_ (repeat i timer) () in
    ignore (tclock 100);
    let (_,iters,elapsed) = run_for_time t0 tclock 10_000 in
    let times = Array.init (BatFloat.ceil (t /. elapsed) |> int_of_float) 
      (fun _ -> tclock iters) 
    in
    Array.map (fun t -> t /. float iters) times
  in 
  print_endline "Warming up";
  let (_,seed,_) = run_for_time 0.1 resolution 10_000 in
  print_endline "Estimating clock resolution";
  let (_,i,clocks) = run_for_time 0.5 resolution seed in
  (* Do we want mean here?!? *)
  let clock_res = analyze_mean i clocks in
  print_endline "Estimating cost of timer call";
  let ts = cost (min (10_000. *. clock_res) 3.) (max 0.01 (5.*.clock_res)) in
  let clock_cost = analyze_mean (Array.length ts) ts in
  {clock_res = clock_res; clock_cost = clock_cost}

type config = { 
  mutable samples: int; 
  mutable resamples: int; 
  mutable gc_between_tests: bool; 
}

let config = { samples=100; resamples = 100_000; gc_between_tests= false }

let time_f ?(env=get_environment ()) (f: int -> 'a) =
  let tclock i = time_ (repeat i timer) () in
  run_for_time 0.1 tclock 10_000 |> ignore;
  let min_time = min (env.clock_res *. 1_000.) 0.1 in
  let (test_time, test_iters, _) = run_for_time min_time f 1 in
  printf "Ran %d iterations in %a\n%!" test_iters print_time test_time;
  let iters = ceil (min_time *. float test_iters /. test_time) in
  let iters_int = int_of_float iters in
  let est_time = float config.samples *. iters *. test_time /. float test_iters in
  printf "Collecting %d samples, %d iterations each, estimated time: %a\n%!"
    config.samples iters_int print_time est_time;
  Array.init config.samples (fun _ -> 
    if config.gc_between_tests then Gc.compact (); 
    time_ f iters_int) |> Array.map (fun t -> t /. iters -. env.clock_cost)

let gen_bench (f, x) = fun i -> repeat i f x

let bench ?env fs = List.map (fun bf -> time_f ?env (gen_bench bf)) fs

let () = 
  let env = get_environment () in
  printf "Clock Res: %a\nClock cost: %a\n" 
    print_time env.clock_res
    print_time env.clock_cost;
  let rec f = function 0 -> 1 | n -> n * f (n-1) in
  bench ~env [f, 100; f, 20]

