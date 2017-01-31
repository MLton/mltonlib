(**
 * This is a signature for computing histogram bucket boundaries.
 *
 * A histogram maps a number of observations into disjoint categories
 * called buckets (or bins).  Normally, the number of buckets is
 * smaller than the number of observations.  The histogram can then be
 * thought of as a lossy abstraction of the original data.
 *
 * For a particular choice of bucket boundaries, the error compared to
 * the original data can be computed with an error function.  A
 * popular error function is sum of squared errors.
 *
 * For a particular error function, observation vector, and number of
 * buckets there is at least one optimal bucketing with the smallest
 * possible total error.
 *
 *)
signature HISTOGRAM = sig
   type e = int * int -> real
   (**
    * This is the type of an error function.  An error function {e
    * (min, max)} returns the error for a bucket in the range [min,
    * max].  A valid error function {e} must satisfy this
    *    {e (i, j) >= e (i, k) + e (k + 1, j)}
    * for any i, j, and k such that 0 <= i <= k <= j <= n, where n is
    * the end boundary of the last bucket (see {compute} below).  In
    * other words, the error function must always return a smaller or
    * equal total value when a bucket is split into smaller buckets.
    *)

   val optimal : {error : e, num : int, range : int} -> real * int list
   (**
    * Computes an optimal histogram for {num} buckets in the range
    * [0, {range}] using the given error function.
    *
    * The length of the returned list is {num - 1}.  Each item in the
    * list signifies the start of a bucket range: for a list of
    * {[a, b, ..., x]} the buckets are 0 to a-1, a to b-1, ..., and
    * x to {range}.
    *)

   val sseFromVector : real vector -> e
   (** Creates a sum-squared-error function from a vector of observations. *)

   val sseFromArray : real array -> e
   (** Creates a sum-squared-error function from an array of observations. *)

   val sse : int * (int -> real) -> e
   (**
    * {sse (n, f)} creates a sum-squared-error function from observations
    * returned by {f 0}, {f 1}, ..., {f n}.
    *)


end
