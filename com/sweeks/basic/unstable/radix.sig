signature RADIX = sig

   type t
   (**
    * A radix used for integer<->string conversions.
    *)

   val bin: t
   val dec: t
   val hex: t
   val oct: t

   val toString: t -> string
   (**
    * toString r returns a human-readable string for r.
    *)

end
