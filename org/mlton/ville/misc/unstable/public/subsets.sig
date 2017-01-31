signature SUBSETS = sig

   val kSubsetsInt : int * int -> int list list
   (**
    * {kSubsetsInt (k, k)} returns a list of all k-subsets of the set
    * of integers in the range [0, n-1].
    *)

   val kSubsets : 'a list * int -> 'a list list
   (** Like {kSubsetsInt} but uses the given list as the set. *)

   val kMultisetsInt : int * int -> int list list
   (**
    * {kMultisetsInt (n, k)} returns a lexicographically ordered list
    * of all k-multisets (or k-bags) of the set of integers in the
    * range [0, n-1].
    *)

   val kMultisets : 'a list * int -> 'a list list
   (** Like {kMultisetsInt} but uses the given list as the set. *)


end
