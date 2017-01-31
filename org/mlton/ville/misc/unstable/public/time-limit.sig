signature TIME_LIMIT = sig
   val spend : Time.t * (unit -> unit) -> unit
   (** {spend (time, f)} calls {f} repeatedly until {time} has passed. *)

   val atMost : Time.t * (unit -> bool) -> bool
   (**
    * {atMost (time, f)} calls {f} repeatedly until it either returns
    * {true} or {time} has passed.  Returns either {true} or {false},
    * respectively.
    *)
end
