signature BIT_FLAGS = sig

   type t
   (**
    * A set of flags.
    *)

   val all: t
   val difference: t * t -> t
   val doIntersect: t * t -> bool
   val fromWord: SysWord.t -> t
   val intersect: t list -> t
   val isSubset: t * t -> bool
   val toWord: t -> SysWord.t
   val union: t list -> t

end
