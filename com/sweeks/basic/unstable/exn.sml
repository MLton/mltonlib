structure Exn: EXN where type t = exn = struct

   type t = exn

   local
      datatype 'a z = Ok of 'a | Raise of exn
   in
      val try: (unit -> 'a) * ('a -> 'b) * (exn -> 'b) -> 'b =
         fn (t, k, h) =>
         case Ok (t ()) handle e => Raise e of
            Ok x => k x
          | Raise e => h e
   end

   fun finally (thunk, cleanup: unit -> unit) =
      try (thunk, fn a => (cleanup (); a), fn e => (cleanup (); raise e))

end

local
   open Exn
in
   val finally = finally
end
