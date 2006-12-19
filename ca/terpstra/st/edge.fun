functor Edge(Poll : EPOLL) :> SCHEDULER_EXTRA =
  struct
    open State
    open Thread_Extra
    open Timeout_Extra
    open Poll
    
    val poll = create 1000 (* ready for 1000 file descriptors *)
    
    structure IoEvent : IOEVENT =
      struct
        open IoEvent
        fun monitor fd status = (
          add (poll, fd);
          IoEvent.monitor fd status)
        fun unmonitor fd = (
          remove (poll, fd);
          IoEvent.unmonitor fd)
      end
    open IoEvent
    
    fun sigPulse thread = thread before stop ()
    
    fun loop block = 
      let
        fun relativeTime time =
          let
            val delta = Time.- (time, Time.now ())
          in
            if Time.< (delta, Time.zeroTime) 
            then Time.zeroTime
            else delta
          end
            
        val delay = 
          case block of
              PENDING => SOME Time.zeroTime
            | COMPLETE => Option.map relativeTime (getNext ())
      in
        wait (poll, delay);
        trigger (Time.now ());
        loop (run ())
      end
    
    fun main () = 
      let
        open MLton
        open Signal
        val real = Itimer.signal Itimer.Real
        val freq = Time.fromMilliseconds 50
      in
        (* prevent high throughput connections from causing starvation *)
        Mask.unblock (Mask.some [real]);
        setHandler (real, Handler.handler sigPulse);
        (* Itimer.set (Itimer.Real, { interval = freq, value = freq }); *)
        loop (run ())
      end
  end
