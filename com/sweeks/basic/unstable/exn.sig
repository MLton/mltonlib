signature EXN = sig

   type t
      
   val finally: 'a thunk * Unit.t thunk -> 'a
   (**
    * finally (f, g) runs f and then g, returning the result of f.  If f raises
    * an exception, the g still runs, after which the exception is re-raised.
    *)
   val try: 'a thunk * ('a -> 'b) * (t -> 'b) -> 'b
   (**
    * try (t, ok, fail) runs t().  If t() yields a value v, then try runs
    * "ok v".  If t() raises an exception e, then try runs "fail e".  The key
    * point is that "ok v" does not run in the context of the fail handler.
    *
    * See "Exceptional Syntax" by Benton and Kennedy:
    *   http://mlton.org/References#BentonKennedy01
    *)

end


