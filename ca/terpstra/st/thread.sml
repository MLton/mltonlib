structure Thread_Extra :> THREAD_EXTRA =
  struct
    open MLton.Thread
    open State
    type thread = Runnable.t
    
    val ready : thread Queue.queue = Queue.new ()
    val loop  : thread option ref = ref NONE
    val quit  : bool ref = ref false
    
    fun next () = 
      if Queue.empty ready orelse !quit then valOf (!loop) else
      valOf (Queue.deque ready)
    
    fun spawn main = 
      Queue.enque (ready, prepare (new 
        (fn () => (main (); switch (fn _ => next ()))), ()))
    
    fun yield result = switch (fn thread => (
      Queue.enque (ready, prepare (thread, result)); 
      next ()))
    
    datatype loop = COMPLETE | PENDING
    fun run () = (
      quit := false;
      switch (fn thread => (loop := SOME (prepare (thread, ())); next ()));
      case Queue.empty ready of
        true => COMPLETE | false => PENDING)
    
    fun stop () = quit := true
    
    (* the while loop deals with the case that a state may have only
     * temporarily become true (before switch), but is not true any longer.
     *)
    fun stopTill state =
      while not (value state) do switch (fn thread => 
        let
          fun resume _ = (
            release state;
            Queue.enque (ready, prepare (thread, ())))
        in
          swatch resume state;
          next ()
        end)
    
    fun select events =
      let
        fun map (state, res) = if value state then SOME res else NONE
        fun block thread =
          let
            fun resume _ = (
              List.app (fn (state, _) => release state) events;
              Queue.enque (ready, prepare (thread, ())))
          in
            List.app (fn (state, _) => swatch resume state) events;
            next ()
          end
      in
        case List.mapPartial map events of
          x :: r => x :: r
        | [] => (switch block; select events)
      end
  end

structure Thread :> THREAD = Thread_Extra
