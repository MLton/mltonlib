signature OPTION = sig
   
   datatype 'a t =
      None
    | Some of 'a
   (**
    * The standard option type (with correct capitalization).
    *)

   val isNone: 'a t -> bool
   (**
    * isNone None = true
    *)
   val isSome: 'a t -> bool
   (**
    * isSome (Some x) = true
    *)
   val map: 'a t * ('a -> 'b) -> 'b t
   (**
    * map (None, f) = None
    * map (Some x, f) = Some (f x)
    *)
   val toSeq: 'a t -> 'a seq
   (**
    * toSeq None returns an empty sequence.
    * toSeq (Some x) returns the sequence [x].
    *)
   val valOf: 'a t -> 'a
   (**
    * valOf None = raise Option
    * valOf (Some x) = x
    *)

end
