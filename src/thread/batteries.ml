(* open this to extend all Foo with BatFoo *)

module Legacy = struct
  include Pervasives
  module Arg = Arg
  module Array = Array
  module ArrayLabels = ArrayLabels
  module Buffer = Buffer
  module Callback = Callback
  module Char = Char
  module Complex = Complex
  module Digest = Digest
  module Filename = Filename
  module Format = Format
  module Gc = Gc
  module Genlex = Genlex
  module Hashtbl = Hashtbl
  module Int32 = Int32
  module Int64 = Int64
  module Lazy = Lazy
  module Lexing = Lexing
  module List = List
  module ListLabels = ListLabels
  module Map = Map
  module Marshal = Marshal
  module MoreLabels = MoreLabels
  module Nativeint = Nativeint
  module Oo = Oo
  module Parsing = Parsing
  module Printexc = Printexc
  module Printf = Printf
  module Queue = Queue
  module Random = Random
  module Scanf = Scanf
  module Set = Set
  module Sort = Sort
  module Stack = Stack
  module StdLabels = StdLabels
  module Stream = Stream
  module String = String
  module StringLabels = StringLabels
  module Sys = Sys
  module Weak = Weak
end

(* stdlib modules *)
module Arg = struct include Arg include BatArg end
module Array = struct include Array include BatArray end
(* ArrayLabels *)
module Buffer = struct include Buffer include BatBuffer end
(* Callback *)
module Char = struct include Char include BatChar end
module Complex = struct include Complex include BatComplex end
module Digest = struct include Digest include BatDigest end
(* Filename *)
module Format = struct include Format include BatFormat end
module Gc = struct include Gc include BatGc end
module Genlex = struct include Genlex include BatGenlex end
module Hashtbl = BatHashtbl
module Int32 = BatInt32
module Int64 = BatInt64
(* Lazy *)
module Lexing = struct include Lexing include BatLexing end
module List = struct include List include BatList end
(* ListLabels *)
module Map = BatMap
module Marshal = struct include Marshal include BatMarshal end
(* MoreLabels *)
module Nativeint = BatNativeint
module Oo = struct include Oo include BatOo end
(* Parsing *)
module Printexc = struct include Printexc include BatPrintexc end
module Printf = struct include Printf include BatPrintf end
module Queue = struct include Queue include BatQueue end
module Random = BatRandom
module Scanf = BatScanf
module Set = BatSet
(* Sort - Deprecated *)
module Stack = struct include Stack include BatStack end
module Stream = struct include Stream include BatStream end
module String = struct include String include BatString end
(* StringLabels *)
module Sys = struct include Sys include BatSys end
(* Weak *)

(* Unix *)
module Unix = struct include Unix include BatUnix end

(* Str *)
module Str = struct include Str include BatStr end

(* Num *)
module Big_int = struct include Big_int include BatBig_int end

(* Bigarray *)
module Bigarray = BatBigarray


(* Extlib modules not replacing stdlib *)
module Base64 = BatBase64
module BitSet = BatBitSet
module Bit_set = BatBitSet
module Dllist = BatDllist
module DynArray = BatDynArray
module Enum = BatEnum
module File = BatFile
module Global = BatGlobal
module IO = BatIO
module LazyList = BatLazyList
module MultiPMap = BatMultiPMap
module Option = BatOption
module OptParse = BatOptParse
module PMap = BatPMap
module PSet = BatPSet
module RefList = BatRefList
module Ref = BatRef
module Std = BatStd

(* NOTE: MAKE SURE TO DUPLICATE CHANGES TO thread/batteries.ml *)
(* Batteries specific modules *)
module Bool = BatBool
module Cache = BatCache
module CharParser = BatCharParser
module Concurrent = BatConcurrent
module Deque = BatDeque
module Float = BatFloat
module Hashcons = BatHashcons
module IMap = BatIMap
module Int = BatInt
module Interfaces = BatInterfaces
module ISet = BatISet
module Logger = BatLogger
module Monad = BatMonad
module Number = BatNumber
module Pair = BatPair
module ParserCo = BatParserCo
module PathGen = BatPathGen
module Print = BatPrint
module Result = BatResult
module Return = BatReturn
module Rope = BatRope
module Seq = BatSeq
module Splay = BatSplay
module Substring = BatSubstring
module Tuple = BatTuple
module Tuple2 = BatTuple.Tuple2
module Tuple3 = BatTuple.Tuple3
module Tuple4 = BatTuple.Tuple4
module Tuple5 = BatTuple.Tuple5
module UChar = struct include BatCamomile.UChar include BatUChar end
module UCharParser = BatUCharParser
module Unit = BatUnit
module Uref = BatUref
module UTF8 = BatUTF8
module ValuePrinter = BatValue_printer
module Vect = BatVect

(* Pervasives *)
include Pervasives
include BatPervasives;;

BatUnix.lock := BatRMutex.make ();;
BatIO.lock := BatRMutex.make ();;
BatIO.lock_factory := BatRMutex.make;;
BatPervasives.lock := BatRMutex.make ();;
