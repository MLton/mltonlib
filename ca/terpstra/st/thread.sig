signature THREAD =
  sig
    (* start a new thread, which will be run later *)
    val spawn: (unit -> unit) -> unit
    val yield: 'a -> 'a (* release control for a tick *)
    
    val stopTill: (bool, 'a) State.state -> unit
    val select: ((bool, 'b) State.state * 'a) list -> 'a list
  end

signature THREAD_EXTRA =
  sig
    include THREAD
    
    datatype loop = COMPLETE | PENDING
    val run:  unit -> loop (* process queue till completed or stopped *)
    val stop: unit -> unit (* stop processing queue and return soon *)
  end
