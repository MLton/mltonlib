(**
 * Signature for functional bit fields, that is, fixed length bit
 * vectors.  This interface allows for an efficient implementation
 * using a single machine word for each bit field.
 *)
signature BIT_FIELD = sig
   type t
   val t : t Generic.Rep.t

   val size : int
   (** Number of bits in a bit field. *)

   val make : bool -> t
   (** Makes a new bit field with all bits set to the given value. *)

   val fromVector : bool vector * bool -> t
   (**
    * {fromVector (v, b)} converts {v} to a bit field.  If {v} is
    * shorter than {size}, the rest of the bits are set to {b}.  If
    * {v} is longer than {size}, and exception is raised.
    *)

   val toVector : t -> bool vector

   val flip : t * int -> t
   (**
    * {flip (v, i)} returns a new bit field made from {v} by flipping
    * the bit at position {i}.
    *)

   val get : t * int -> bool
   (** {get (v, i)} returns the in the bit field {v} at position {i}. *)

   val allZero : t -> bool
   (** Returns a true value if all bits are zero. *)

   val findOne : t * int -> int
   (**
    * {find (v, i)} returns the next position after position {i} that
    * has the bit set to one.
    *)

   val count : t -> int
   (** Returns the number of bits set to one. *)

   val foldi : (int * bool * 'a -> 'a) -> 'a -> t -> 'a

end

(* A bit field which is explicitly implemented with a {WORD} type. *)
signature BIT_FIELD_W = sig
   include BIT_FIELD
   type word
   val fromWord : word -> t
   val toWord : t -> word
end
