(*
 * ExtSet - Extended operations on sets
 * Copyright (C) 1996 Xavier Leroy
 *               2009 David Rajchenbach-Teller, LIFO, Universite d'Orleans
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

(** Sets over ordered types.

    This module implements the set data structure, given a total
    ordering function over the set elements. All operations over sets
    are purely applicative (no side-effects).  The implementation uses
    balanced binary trees, and is therefore reasonably efficient:
    insertion and membership take time logarithmic in the size of the
    set, for instance.

    {b Note} OCaml, Batteries Included, provides two implementations
    of sets: polymorphic sets and functorized sets. Functorized sets
    (see {!S} and {!Make}) are slightly more complex to use but offer
    stronger type-safety. Polymorphic sets make it easier to shoot
    yourself in the foot. In case of doubt, you should use functorized
    sets.

    The functorized set implementation is built upon Stdlib's
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Set.html}Set}
    module, but provides the complete interface.

    @author Xavier Leroy
    @author Nicolas Cannasse
    @author Markus Mottl
    @author David Rajchenbach-Teller
*)

(** {4 Functorized Sets} *)

module type OrderedType = BatInterfaces.OrderedType
(** Input signature of the functor {!Set.Make}. *)

module type S =
  sig
    type elt
    (** The type of the set elements. *)

    type t
    (** The type of sets. *)

    val empty: t
    (** The empty set. *)

    val is_empty: t -> bool
    (** Test whether a set is empty or not. *)

    val singleton: elt -> t
    (** [singleton x] returns the one-element set containing only [x]. *)

    val mem: elt -> t -> bool
    (** [mem x s] tests whether [x] belongs to the set [s]. *)

    val add: elt -> t -> t
    (** [add x s] returns a set containing all elements of [s],
       plus [x]. If [x] was already in [s], [s] is returned unchanged. *)

    val remove: elt -> t -> t
    (** [remove x s] returns a set containing all elements of [s],
       except [x]. If [x] was not in [s], [s] is returned unchanged. *)

    val union: t -> t -> t
    (** Set union. *)

    val inter: t -> t -> t
    (** Set intersection. *)

    val diff: t -> t -> t
    (** Set difference. *)

    val sdiff: t -> t -> t
    (** [sdiff s t] returns the set of all elements in [s] or [t] but not both.
       This is the same as [diff (union s t) (inter s t)]. *)

    val compare: t -> t -> int
    (** Total ordering between sets. Can be used as the ordering function
       for doing sets of sets. *)

    val equal: t -> t -> bool
    (** [equal s1 s2] tests whether the sets [s1] and [s2] are
       equal, that is, contain equal elements. *)

    val subset: t -> t -> bool
    (** [subset s1 s2] tests whether the set [s1] is a subset of
       the set [s2]. *)

    val disjoint: t -> t -> bool
    (** [disjoint s1 s2] tests whether the sets [s1] and [s2] contain no shared
        elements. (i.e. [inter s1 s2] is empty.) *)

    val compare_subset: t -> t -> int
    (** Partial ordering between sets as generated by [subset] *)

    val iter: (elt -> unit) -> t -> unit
    (** [iter f s] applies [f] in turn to all elements of [s].
       The elements of [s] are presented to [f] in increasing order
       with respect to the ordering over the type of the elements. *)

    val map: (elt -> elt) -> t -> t
      (** [map f x] creates a new set with elements [f a0],
	  [f a1]... [f aN], where [a0],[a1]..[aN] are the
	  values contained in [x]*)

    val filter: (elt -> bool) -> t -> t
    (** [filter p s] returns the set of all elements in [s]
       that satisfy predicate [p]. *)

    val filter_map: (elt -> elt option) -> t -> t
      (** [filter_map f m] combines the features of [filter] and
	  [map].  It calls calls [f a0], [f a1], [f aN] where [a0],[a1]..[aN]
	  are the elements of [m] and returns the set of pairs [bi]
	  such as [f ai = Some bi] (when [f] returns [None], the
	  corresponding element of [m] is discarded). *)

    val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
      (** [fold f s a] computes [(f xN ... (f x1 (f x0 a))...)],
	  where [x0],[x1]..[xN] are the elements of [s], in increasing order. *)

    val for_all: (elt -> bool) -> t -> bool
    (** [for_all p s] checks if all elements of the set
       satisfy the predicate [p]. *)

    val exists: (elt -> bool) -> t -> bool
    (** [exists p s] checks if at least one element of
       the set satisfies the predicate [p]. *)

    val partition: (elt -> bool) -> t -> t * t
    (** [partition p s] returns a pair of sets [(s1, s2)], where
       [s1] is the set of all the elements of [s] that satisfy the
       predicate [p], and [s2] is the set of all the elements of
       [s] that do not satisfy [p]. *)

    val split: elt -> t -> t * bool * t
    (** [split x s] returns a triple [(l, present, r)], where
          [l] is the set of elements of [s] that are
          strictly less than [x];
          [r] is the set of elements of [s] that are
          strictly greater than [x];
          [present] is [false] if [s] contains no element equal to [x],
          or [true] if [s] contains an element equal to [x]. *)

    val cardinal: t -> int
    (** Return the number of elements of a set. *)

    val elements: t -> elt list
    (** Return the list of all elements of the given set.
       The returned list is sorted in increasing order with respect
       to the ordering [Ord.compare], where [Ord] is the argument
       given to {!Set.Make}. *)

    val min_elt: t -> elt
    (** Return the smallest element of the given set
       (with respect to the [Ord.compare] ordering).

	@raise Not_found if the set is empty. *)

    val max_elt: t -> elt
    (** Same as {!Set.S.min_elt}, but returns the largest element of the
       given set. *)

    val choose: t -> elt
    (** Return one element of the given set, or raise [Not_found] if
       the set is empty. Which element is chosen is unspecified,
       but equal elements will be chosen for equal sets. *)

    val pop : t -> elt * t
    (** returns one element of the set and the set without that element.
        Raises [Not_found] if given an empty set *)

    val enum: t -> elt BatEnum.t
      (** Return an enumeration of all elements of the given set.
	  The returned enumeration is sorted in increasing order with respect
	  to the ordering [Ord.compare], where [Ord] is the argument
	  given to {!Set.Make}. *)

    val backwards: t -> elt BatEnum.t
      (** Return an enumeration of all elements of the given set.
	  The returned enumeration is sorted in decreasing order with respect
	  to the ordering [Ord.compare], where [Ord] is the argument
	  given to {!Set.Make}. *)

    val of_enum: elt BatEnum.t -> t


    (** {6 Boilerplate code}*)

    (** {7 Printing}*)

    val print :  ?first:string -> ?last:string -> ?sep:string ->
      ('a BatInnerIO.output -> elt -> unit) ->
      'a BatInnerIO.output -> t -> unit


      (** {6 Override modules}*)

    (**
       The following modules replace functions defined in {!Set} with functions
       behaving slightly differently but having the same name. This is by design:
       the functions meant to override the corresponding functions of {!Set}.
    *)

    (** Operations on {!Set} without exceptions.*)
    module Exceptionless : sig
      val min_elt: t -> elt option
      val max_elt: t -> elt option
      val choose:  t -> elt option
    end


    (** Operations on {!Set} with labels.

	This module overrides a number of functions of {!Set} by
	functions in which some arguments require labels. These labels are
	there to improve readability and safety and to let you change the
	order of arguments to functions. In every case, the behavior of the
	function is identical to that of the corresponding function of {!Set}.
    *)
    module Labels : sig
      val iter : f:(elt -> unit) -> t -> unit
      val fold : f:(elt -> 'a -> 'a) -> t -> init:'a -> 'a
      val for_all : f:(elt -> bool) -> t -> bool
      val exists : f:(elt -> bool) -> t -> bool
      val map: f:(elt -> elt) -> t -> t
      val filter : f:(elt -> bool) -> t -> t
      val filter_map: f:(elt -> elt option) -> t -> t
      val partition : f:(elt -> bool) -> t -> t * t
    end

  end
(** Output signature of the functor {!Set.Make}. *)

(*
module IStringSet : S with type elt = String.t
(** A set of strings. Comparison of strings ignores case (i.e. "foo" = "Foo")*)

module NumStringSet : S with type elt = String.t
(** A set of strings. Comparison of strings takes into account embedded numbers (i.e. "a23" < "a123", "a01" = "a1") *)

module RopeSet    : S with type elt = BatRope.t
(** A set of ropes. Comparison of ropes takes case into account (i.e. r"foo" <> r"Foo")*)

module IRopeSet   : S with type elt = BatRope.t
(** A set of ropes. Comparison of ropes ignores case (i.e. r"foo" = r"Foo")*)

 *)

module Make (Ord : OrderedType) : S with type elt = Ord.t
(** Functor building an implementation of the set structure
    given a totally ordered type.

    @documents Set.Make
*)

(** {6 Polymorphic sets}

   The definitions below describe the polymorphic set interface.

    They are similar in functionality to the functorized {!Make}
    module, but the compiler cannot ensure that sets using different
    element ordering have different types: the responsibility of not
    mixing non-sensical comparison functions together is to the
    programmer. If in doubt, you should rather use the {!Make}
    functor for additional safety.

   @author Nicolas Cannasse
   @author Markus Mottl
   @author David Rajchenbach-Teller
*)

type 'a t
(** The type of sets. *)

include BatEnum.Enumerable with type 'a enumerable = 'a t
include BatInterfaces.Mappable with type 'a mappable = 'a t

val empty: 'a t
  (** The empty set, using [compare] as comparison function *)

val is_empty: 'a t -> bool
  (** Test whether a set is empty or not. *)

val singleton : 'a -> 'a t
(** Creates a new set with the single given element in it. *)

val mem: 'a -> 'a t -> bool
  (** [mem x s] tests whether [x] belongs to the set [s]. *)

val add: 'a -> 'a t -> 'a t
  (** [add x s] returns a set containing all elements of [s],
      plus [x]. If [x] was already in [s], [s] is returned unchanged. *)

val remove: 'a -> 'a t -> 'a t
  (** [remove x s] returns a set containing all elements of [s],
      except [x]. If [x] was not in [s], [s] is returned unchanged. *)

val union: 'a t -> 'a t -> 'a t
  (** [union s t] returns the union of [s] and [t] - the set containing
      all elements in either [s] and [t].  The returned set uses [t]'s
      comparison function.  The current implementation works better for
      small [s]. *)

(* Set.Make uses intersect *)
val intersect: 'a t -> 'a t -> 'a t
(** [intersect s t] returns a new set of those elements that are in
    both [s] and [t].  The returned set uses [s]'s comparison function. *)

val diff: 'a t -> 'a t -> 'a t
  (** [diff s t] returns the set of all elements in [s] but not in
      [t]. The returned set uses [s]'s comparison function.*)

val sdiff: 'a t -> 'a t -> 'a t
(** [sdiff s t] returns the set of all elements in [s] or [t] but not both.
    This is the same as [diff (union s t) (inter s t)]. The returned set uses
    [s]'s comparison function.*)

val compare: 'a t -> 'a t -> int
(** Total ordering between sets. Can be used as the ordering function
    for doing sets of sets. *)

val equal: 'a t -> 'a t -> bool
(** [equal s1 s2] tests whether the sets [s1] and [s2] are
    equal, that is, contain equal elements. *)

val subset: 'a t -> 'a t -> bool
(** [subset a b] returns true if [a] is a subset of [b]. O(|a|). *)

val disjoint: 'a t -> 'a t -> bool
(** [disjoint s1 s2] tests whether the sets [s1] and [s2] contain no
    shared elements. (i.e. [inter s1 s2] is empty.) *)

val iter: ('a -> unit) -> 'a t -> unit
  (** [iter f s] applies [f] in turn to all elements of [s].
      The elements of [s] are presented to [f] in increasing order
      with respect to the ordering over the type of the elements. *)

(* under-specified; either give a 'b comparison,
   or keep ('a -> 'a) (preferred choice) *)
val map: ('a -> 'b) -> 'a t -> 'b t
  (** [map f x] creates a new set with elements [f a0],
      [f a1]... [f aN], where [a0], [a1], ..., [aN] are the
      values contained in [x]

      The resulting map uses the polymorphic [compare] function to
      order elements.
  *)

val filter: ('a -> bool) -> 'a t -> 'a t
(** [filter p s] returns the set of all elements in [s]
    that satisfy predicate [p]. *)

(* as under-specified as 'map' *)
val filter_map: ('a -> 'b option) -> 'a t -> 'b t
  (** [filter_map f m] combines the features of [filter] and
      [map].  It calls calls [f a0], [f a1], [f aN] where [a0,a1..an]
      are the elements of [m] and returns the set of pairs [bi]
      such as [f ai = Some bi] (when [f] returns [None], the
      corresponding element of [m] is discarded).

      The resulting map uses the polymorphic [compare] function to
      order elements.
  *)

val fold: ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** [fold f s a] computes [(f xN ... (f x1 (f x0 a))...)],
    where [x0,x1..xN] are the elements of [s], in increasing order. *)

val exists: ('a -> bool) -> 'a t -> bool
  (** [exists p s] checks if at least one element of
      the set satisfies the predicate [p]. *)

val for_all : ('a -> bool) -> 'a t -> bool
(** Returns whether the given predicate applies to all elements in the set *)

val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
  (** returns two disjoint subsets, those that satisfy the given
      predicate and those that don't *)

val split : 'a -> 'a t -> 'a t * bool * 'a t
  (** [split x s] returns a triple [(l, present, r)], where
      [l] is the set of elements of [s] that are
      strictly less than [x];
      [r] is the set of elements of [s] that are
      strictly greater than [x];
      [present] is [false] if [s] contains no element equal to [x],
      or [true] if [s] contains an element equal to [x]. *)

val cardinal: 'a t -> int
  (** Return the number of elements of a set. *)

val elements: 'a t -> 'a list
(** Return the list of all elements of the given set.
    The returned list is sorted in increasing order with respect
    to the ordering of the given set. *)

val min_elt : 'a t -> 'a
  (** returns the smallest element of the set. Raises
      [Invalid_argument] if given an empty set. *)

val max_elt : 'a t -> 'a
  (** returns the largest element of the set. Raises
      [Invalid_argument] if given an empty set.*)

val choose : 'a t -> 'a
  (** returns an arbitrary (but deterministic) element of the given set.
      Raises [Invalid_argument] if given an empty set. *)

val pop : 'a t -> 'a * 'a t
  (** returns one element of the set and the set without that element.
      Raises [Not_found] if given an empty set *)

val enum: 'a t -> 'a BatEnum.t
  (** Return an enumeration of all elements of the given set.
      The returned enumeration is sorted in increasing order with respect
      to the ordering of this set.*)

val of_enum: 'a BatEnum.t -> 'a t

(* of_list has no Set.Make counterpart *)
val of_list: 'a list -> 'a t
(** builds a set from the given list, using the default comparison
    function *)

(** {6 Boilerplate code}*)


(** {7 Printing}*)

val print :  ?first:string -> ?last:string -> ?sep:string ->
  ('a BatInnerIO.output -> 'c -> unit) ->
  'a BatInnerIO.output -> 'c t -> unit


module PSet : sig
  (** {6 Polymorphic sets}

      The definitions below describe the polymorphic set interface.

      They are similar in functionality to the functorized
      {!BatSet.Make} module, but the compiler cannot ensure that sets
      using different element ordering have different types: the
      responsibility of not mixing non-sensical comparison functions
      together is to the programmer. If you ever need a custom
      comparison function, it is recommended to use the {!BatSet.Make}
      functor for additional safety.

      @author Nicolas Cannasse
      @author Markus Mottl
      @author David Rajchenbach-Teller
   *)

  type 'a t
  (** The type of sets. *)

  include BatEnum.Enumerable with type 'a enumerable = 'a t
  include BatInterfaces.Mappable with type 'a mappable = 'a t

  val empty: 'a t
  (** The empty set, using [compare] as comparison function *)

  val create : ('a -> 'a -> int) -> 'a t
  (** Creates a new empty set, using the provided function for key comparison.*)

  val is_empty: 'a t -> bool
  (** Test whether a set is empty or not. *)

  val singleton : ?cmp:('a -> 'a -> int) -> 'a -> 'a t
  (** Creates a new set with the single given element in it. *)

  val mem: 'a -> 'a t -> bool
  (** [mem x s] tests whether [x] belongs to the set [s]. *)

  val add: 'a -> 'a t -> 'a t
  (** [add x s] returns a set containing all elements of [s],
      plus [x]. If [x] was already in [s], [s] is returned unchanged. *)

  val remove: 'a -> 'a t -> 'a t
  (** [remove x s] returns a set containing all elements of [s],
      except [x]. If [x] was not in [s], [s] is returned unchanged. *)

  val union: 'a t -> 'a t -> 'a t
  (** [union s t] returns the union of [s] and [t] - the set containing
      all elements in either [s] and [t].  The returned set uses [t]'s
      comparison function.  The current implementation works better for
      small [s]. *)

  (* Set.Make uses intersect *)
  val intersect: 'a t -> 'a t -> 'a t
  (** [intersect s t] returns a new set of those elements that are in
      both [s] and [t].  The returned set uses [s]'s comparison function. *)

  val diff: 'a t -> 'a t -> 'a t
  (** [diff s t] returns the set of all elements in [s] but not in
      [t]. The returned set uses [s]'s comparison function.*)

  val sdiff: 'a t -> 'a t -> 'a t
  (** [sdiff s t] returns the set of all elements in [s] or [t] but not both.
      This is the same as [diff (union s t) (inter s t)]. The returned set uses
      [s]'s comparison function.*)

  val compare: 'a t -> 'a t -> int
  (** Total ordering between sets. Can be used as the ordering function
      for doing sets of sets. *)

  val equal: 'a t -> 'a t -> bool
  (** [equal s1 s2] tests whether the sets [s1] and [s2] are
      equal, that is, contain equal elements. *)

  val subset: 'a t -> 'a t -> bool
  (** [subset a b] returns true if [a] is a subset of [b]. O(|a|). *)

  val disjoint: 'a t -> 'a t -> bool
  (** [disjoint s1 s2] tests whether the sets [s1] and [s2] contain no
      shared elements. (i.e. [inter s1 s2] is empty.) *)

  val iter: ('a -> unit) -> 'a t -> unit
  (** [iter f s] applies [f] in turn to all elements of [s].
      The elements of [s] are presented to [f] in increasing order
      with respect to the ordering over the type of the elements. *)

  (* under-specified; either give a 'b comparison,
     or keep ('a -> 'a) (preferred choice) *)
  val map: ('a -> 'b) -> 'a t -> 'b t
  (** [map f x] creates a new set with elements [f a0],
      [f a1]... [f aN], where [a0], [a1], ..., [aN] are the
      values contained in [x]

      The resulting map uses the polymorphic [compare] function to
      order elements.
   *)

  val filter: ('a -> bool) -> 'a t -> 'a t
  (** [filter p s] returns the set of all elements in [s]
      that satisfy predicate [p]. *)

  (* as under-specified as 'map' *)
  val filter_map: ('a -> 'b option) -> 'a t -> 'b t
  (** [filter_map f m] combines the features of [filter] and
      [map].  It calls calls [f a0], [f a1], [f aN] where [a0,a1..an]
      are the elements of [m] and returns the set of pairs [bi]
      such as [f ai = Some bi] (when [f] returns [None], the
      corresponding element of [m] is discarded).

      The resulting map uses the polymorphic [compare] function to
      order elements.
   *)

  val fold: ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold f s a] computes [(f xN ... (f x1 (f x0 a))...)],
      where [x0,x1..xN] are the elements of [s], in increasing order. *)

  val exists: ('a -> bool) -> 'a t -> bool
  (** [exists p s] checks if at least one element of
      the set satisfies the predicate [p]. *)

  val for_all : ('a -> bool) -> 'a t -> bool
  (** Returns whether the given predicate applies to all elements in the set *)

  val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
  (** returns two disjoint subsets, those that satisfy the given
      predicate and those that don't *)

  val split : 'a -> 'a t -> 'a t * bool * 'a t
  (** [split x s] returns a triple [(l, present, r)], where
      [l] is the set of elements of [s] that are
      strictly less than [x];
      [r] is the set of elements of [s] that are
      strictly greater than [x];
      [present] is [false] if [s] contains no element equal to [x],
      or [true] if [s] contains an element equal to [x]. *)

  val cardinal: 'a t -> int
  (** Return the number of elements of a set. *)

  val elements: 'a t -> 'a list
  (** Return the list of all elements of the given set.
      The returned list is sorted in increasing order with respect
      to the ordering of the given set. *)

  val min_elt : 'a t -> 'a
  (** returns the smallest element of the set. Raises
      [Invalid_argument] if given an empty set. *)

  val max_elt : 'a t -> 'a
  (** returns the largest element of the set. Raises
      [Invalid_argument] if given an empty set.*)

  val choose : 'a t -> 'a
  (** returns an arbitrary (but deterministic) element of the given set.
      Raises [Invalid_argument] if given an empty set. *)

  val pop : 'a t -> 'a * 'a t
  (** returns one element of the set and the set without that element.
      Raises [Not_found] if given an empty set *)

  val enum: 'a t -> 'a BatEnum.t
  (** Return an enumeration of all elements of the given set.
      The returned enumeration is sorted in increasing order with respect
      to the ordering of this set.*)

  val of_enum: 'a BatEnum.t -> 'a t

  val of_enum_cmp: cmp:('a -> 'a -> int) -> 'a BatEnum.t -> 'a t

  (* of_list has no Set.Make counterpart *)
  val of_list: 'a list -> 'a t
  (** builds a set from the given list, using the default comparison
      function *)

  (** {6 Boilerplate code}*)


  (** {7 Printing}*)

  val print :  ?first:string -> ?last:string -> ?sep:string ->
    ('a BatInnerIO.output -> 'c -> unit) ->
    'a BatInnerIO.output -> 'c t -> unit
end
