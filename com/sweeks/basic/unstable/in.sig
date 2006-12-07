signature IN = sig

   type t
   (**
    * The type of input streams.  See File.{openIn,withIn} for functions that
    * open files as instreams.
    *)

   val close: t -> unit
   (**
    * close s closes input stream s.
    *)
   val lines: t -> string seq
   (**
    * lines s returns a sequence of the lines in s.  The sequence is delayed.
    *)
   val get1: t -> char option
   (**
    * get1 s gets a single character from s.
    *)
   val getAll: t -> string
   (**
    * getAll s returns a string containing the rest of the characters in s.
    *)
   val getLine: t -> string option
   (**
    * getLine s consumes a single line of s.
    *)
   val standard: t
   (**
    * The standard input stream (stdin).
    *)

end
