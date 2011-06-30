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

(* TODO: make customizable timer? *)
let timer = Sys.time

let rec repeat n f x = if n = 0 then () else ignore (f x); repeat (n-1) f x

let time f x = 
  let t0 = timer () in
  let ret = f x in
  timer () -. t0, ret

let run_for_time t seed0 f = 
  let t0 = timer () in
  let rec loop seed iters =
    let now = timer() in
    if now -. t0 > t *. 10. then 
      failwith "Took too long to run: seed %d iters %d" seed iters;
    let ti, ret = time f seed in
    if ti > t then (ti, seed, ret)
    else loop (2*seed) (iters+1)
  in
  loop seed0 0

let mean a = Array.reduce (+.) a /. float (Array.length a)

let analyze_mean _i a = let _m = mean a in assert false


let get_timer_granularity () =
  let resolution i = 
    let times = Array.init (i+1) (fun _ -> timer()) in
    let pos_diffs = Array.init i (fun i -> times.(i+1) -. times.(i)) |> Array.filter (> 0.) in
    pos_diffs
  in
  let cost t = 
    let tclock i = time (repeat i timer ()) in
    ignore (tclock 1);
    let (_,iters,(elapsed,())) = run_for_time 0.01 10000 tclock

  in
  print_endline "Warming up";
  let (_,seed,_) = run_for_time 0.1 10_000 resolution in
  print_endline "Estimating clock resolution";
  let (_,i,clocks) = run_for_time 0.5 seed resolution in
  (* Do we want mean here?!? *)
  let clock_res = analyze_mean clock_diffs i in
  print_endline "Estimating cost of timer call";
  let (a,i) = cost (min (10000. *. clock_res) 1.) in
  let clock_cost = analyze_mean a i in
  
  
