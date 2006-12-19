signature SCHEDULER =
  sig
    val main: unit -> unit
  end

signature SCHEDULER_EXTRA =
  sig
    include SCHEDULER    
    structure IoEvent: IOEVENT
  end
