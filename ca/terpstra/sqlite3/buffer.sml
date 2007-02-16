structure Buffer :> BUFFER =
   struct
      type 'a t = 'a option array ref * int ref
      
      fun empty () = (ref (Array.tabulate (32, fn _ => NONE)), ref 0)
      
      fun subOpt ((a, s), i) = if i >= !s then NONE else Array.sub (!a, i)
      fun sub (a, i) = valOf (subOpt (a, i))
      
      fun double (a, s) =
         a := Array.tabulate (!s * 2, fn i => subOpt ((a, s), i))
      
      fun push ((a, s), v) = (
         if !s = Array.length (!a) then double (a, s) else ();
         Array.update (!a, !s, SOME v);
         !s before s := !s + 1
         )
      
      fun update ((a, _), i, v) = Array.update (!a, i, SOME v)
      
      fun free ((a, _), i) = () (* !!! fixme !!! *)
   end
