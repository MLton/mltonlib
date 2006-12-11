signature OUT = sig

   type t
   (**
    * The type of output streams.  See File.{openOut,withOut} for functions that
    * open files as outstreams.
    *)

   val close: t -> Unit.t
   (**
    * close s closes output stream s.
    *)
   val error: t
   (**
    * The standard error stream (stderr).
    *)
   val flush: t -> Unit.t
   (**
    * flush s causes any buffers associated with s to be written out. 
    *)
   val newline: t -> Unit.t
   (**
    * newline s = put1 (s, #"\n")
    *)
   val put: t * String.t -> Unit.t
   (**
    * put (s, str) outputs the string str on s.
    *)
   val put1: t * Char.t -> Unit.t
   (**
    * put (s, c) outputs the character c on s.
    *)
   val puts: t * String.t Seq.t -> Unit.t
   (**
    * puts (s, ss) = for (ss, fn str => put (s, str))
    *)
   val standard: t
   (**
    * The standard output stream (stdout).
    *)

end
