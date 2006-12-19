(* !!! fixme: timers persist in the heap even if unreferenced.
 * once MLton bug is fixed, use MLton.Weak and MLton.Finalizable
 *)
structure Timeout_Extra :> TIMEOUT_EXTRA =
  struct
    open State
    open Time
    open Heap
    
    type sleeper = time * bool signal
    fun nextSleeper ((t1, _), (t2, _)) = t1 < t2
    val sleeper = new nextSleeper
    val rLastTick = ref (Time.now ())
    
    fun lastTick () = !rLastTick
    
    fun LATERTHAN time =
      let
        val (state, signal) = state false
      in
        push (sleeper, (time, signal));
        state
      end
      
    fun TIMEOUT time = LATERTHAN (time + lastTick ())
    
    fun getNext () =
      case peek sleeper of
          NONE => NONE
        | SOME (t, _) => SOME t
    
    fun trigger time =
      let
        fun loop () =
          case peek sleeper of
              NONE => ()
            | SOME (t, s) => 
                if time < t then () else
                (pop sleeper; s true; loop ())
      in
        rLastTick := time;
        loop ()
      end
  end

structure Timeout :> TIMEOUT = Timeout_Extra
