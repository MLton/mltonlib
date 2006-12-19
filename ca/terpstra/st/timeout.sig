signature TIMEOUT =
  sig
    (* TIMEOUT is measured since the last IO poll, not the instant called *)
    val TIMEOUT:   Time.time -> (bool, bool) State.state
    
    (* LATERTHAN is an absolute time value *)
    val LATERTHAN: Time.time -> (bool, bool) State.state
    
    (* What is the cached time as of last tick (fast) *)
    val lastTick: unit -> Time.time
  end

signature TIMEOUT_EXTRA =
  sig
    include TIMEOUT
    
    (* The earliest pending timer (if any) *)
    val getNext: unit -> Time.time option
    (* Toggle all states to true prior to the given *)
    val trigger: Time.time -> unit
  end
