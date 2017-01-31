structure TimeLimit :> TIME_LIMIT = struct

   fun atMost (time, f) = let
      val timer = Timer.startRealTimer ()
      fun timedOut () = Time.>= (Timer.checkRealTimer timer, time)
      fun loop () =
	  if f () then
	     true
	  else if timedOut () then
	     false
	  else
	     loop ()
   in
      loop ()
   end

   fun spend (time, f) = ignore (atMost (time, fn () => (f () ; false)))

end
