(**
 * A signature for managing command line options.
 *)
signature CMDLINE_OPTS = sig

   type t    (** Type of a command line option collection. *)
   type opt  (** Type of a single command line option. *)

   datatype arg = REQUIRED | NONE | OPTIONAL

   (**= Combinators for building collections of options =*)

   val opt : {arg : arg,
	      func : string option * 'a -> 'a,
	      long : string option,
	      short : char option,
	      description : string}
	     -> opt
   (**
    * Creates a command line option.  The record fields are:
    * - arg: Whether the option takes arguments.  The argument is
    *   either required, must not be present, or optional.
    * - long: The long name of the option
    * - short: The short name of the option
    * - description: An human readable description of what the command
    *   line option does.
    * If both {short} and {long} are none, this will throw an exception.
    * The description text should be written without special whitespace
    * for formatting.  The {usage} function will pretty-print the
    * descriptions automatically.
    *)

   val empty : t
   (** An empty collection contains no options. *)

   val add : t * opt -> t
   (**
    * Adds an option in the collection.  This throws an exception if
    * the short or long option name is already used.
    *)

   val usage : t -> string
   (**
    * Returns a pretty-printed usage string, formatted to a width of
    * at most 75 characters.
    *)

   val parse : t * string list * 'a -> 'a

end
