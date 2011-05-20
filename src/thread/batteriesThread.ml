(* Extras for threading *)
module Mutex = struct include Mutex include BatMutex end;;
module RMutex = BatRMutex;;

