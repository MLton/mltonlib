structure Exn: EXN = struct

   type t = Exn.t

   local
      datatype 'a z = Ok of 'a | Raise of t
   in
      val try: (Unit.t -> 'a) * ('a -> 'b) * (t -> 'b) -> 'b =
         fn (t, k, h) =>
         case Ok (t ()) handle e => Raise e of
            Ok x => k x
          | Raise e => h e
   end

   fun finally (thunk, cleanup: Unit.t -> Unit.t) =
      try (thunk, fn a => (cleanup (); a), fn e => (cleanup (); raise e))

end

local
   open Exn
in
   val finally = finally
end
